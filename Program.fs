open System
open System.IO
open System.Globalization
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection

// ---------- Helpers to read Linux stats ----------
module Linux =
    let tryReadAllText (path: string) =
        try Some (File.ReadAllText(path).Trim())
        with _ -> None

    // CPU temperature (Raspberry Pi): millidegree C
    let cpuTempC () =
        match tryReadAllText "/sys/class/thermal/thermal_zone0/temp" with
        | Some s ->
            match Double.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture) with
            | true, v -> Some (v / 1000.0)
            | _ -> None
        | None -> None

    // /proc/loadavg -> 1, 5, 15 minute load averages
    let loadAvg () =
        match tryReadAllText "/proc/loadavg" with
        | Some s ->
            let parts = s.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            if parts.Length >= 3 then
                let parse i =
                    match Double.TryParse(parts[i], NumberStyles.Any, CultureInfo.InvariantCulture) with
                    | true, v -> Some v
                    | _ -> None
                Some (parse 0, parse 1, parse 2)
            else None
        | None -> None

    // /proc/meminfo -> MemTotal, MemAvailable (kB)
    let memInfo () =
        match tryReadAllText "/proc/meminfo" with
        | None -> None
        | Some txt ->
            let lines = txt.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            let tryGet (key: string) =
                lines
                |> Array.tryFind (fun l -> l.StartsWith(key))
                |> Option.bind (fun l ->
                    // "MemTotal:       948576 kB"
                    let parts = l.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                    if parts.Length >= 2 then
                        match Int64.TryParse(parts[1], NumberStyles.Any, CultureInfo.InvariantCulture) with
                        | true, v -> Some v
                        | _ -> None
                    else None
                )
            match tryGet "MemTotal:" , tryGet "MemAvailable:" with
            | Some totalKb, Some availKb -> Some (totalKb, availKb)
            | _ -> None

    // Disk usage for root filesystem
    let diskRoot () =
        try
            let d = DriveInfo("/")
            Some (d.TotalSize, d.AvailableFreeSpace)
        with _ -> None

    // Network bytes from /proc/net/dev (rx, tx) for a given interface like "eth0" or "wlan0"
    let netBytes (iface: string) =
        match tryReadAllText "/proc/net/dev" with
        | None -> None
        | Some txt ->
            let lines = txt.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            lines
            |> Array.tryFind (fun l -> l.TrimStart().StartsWith(iface + ":"))
            |> Option.bind (fun l ->
                // iface: rxbytes ... txbytes ...
                let cleaned = l.Replace(":", " ")
                let parts = cleaned.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                // parts[1] = rx bytes, parts[9] = tx bytes (Linux proc format)
                if parts.Length >= 10 then
                    match Int64.TryParse(parts[1]), Int64.TryParse(parts[9]) with
                    | (true, rx), (true, tx) -> Some (rx, tx)
                    | _ -> None
                else None
            )
// ---------- In-memory state ----------
type Sample =
    { TimestampUtc: DateTime
      CpuTempC: float option
      Load1: float option
      Load5: float option
      Load15: float option
      MemTotalKb: int64 option
      MemAvailKb: int64 option
      DiskTotalBytes: int64 option
      DiskAvailBytes: int64 option
      RxBytes: int64 option
      TxBytes: int64 option
      Alerts: string list }

type TelemetryState() =
    let gate = obj()
    let mutable last : Sample option = None
    member _.Get() =
        lock gate (fun () -> last)
    member _.Set(v: Sample) =
        lock gate (fun () -> last <- Some v)
