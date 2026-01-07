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
// ---------- Background sampler (hosted service) ----------
type Sampler(state: TelemetryState, iface: string) =
    inherit BackgroundService()

    override _.ExecuteAsync(stoppingToken: CancellationToken) : Task =
        task {
            while not stoppingToken.IsCancellationRequested do
                let now = DateTime.UtcNow

                let temp = Linux.cpuTempC()
                let load = Linux.loadAvg()
                let mem = Linux.memInfo()
                let disk = Linux.diskRoot()
                let net = Linux.netBytes iface

                let (l1, l5, l15) =
                    match load with
                    | Some (a,b,c) -> (a, b, c)
                    | None -> (None, None, None)

                let (mt, ma) =
                    match mem with
                    | Some (t,a) -> (Some t, Some a)
                    | None -> (None, None)

                let (dt, da) =
                    match disk with
                    | Some (t,a) -> (Some t, Some a)
                    | None -> (None, None)

                let (rx, tx) =
                    match net with
                    | Some (r,t) -> (Some r, Some t)
                    | None -> (None, None)

                // Simple alerts (threshold based)
                let alerts =
                    [ match temp with
                      | Some t when t >= 75.0 -> yield (sprintf "High CPU temp: %.1fC" t)
                      | _ -> ()

                      match ma, mt with
                      | Some avail, Some total when total > 0L ->
                          let usedPct = 100.0 * (1.0 - float avail / float total)
                          if usedPct >= 90.0 then yield (sprintf "High memory usage: %.1f%%" usedPct)
                      | _ -> ()

                      match da, dt with
                      | Some availB, Some totalB when totalB > 0L ->
                          let usedPct = 100.0 * (1.0 - float availB / float totalB)
                          if usedPct >= 90.0 then yield (sprintf "Low disk space: %.1f%% used" usedPct)
                      | _ -> () ]

                let sample =
                    { TimestampUtc = now
                      CpuTempC = temp
                      Load1 = l1; Load5 = l5; Load15 = l15
                      MemTotalKb = mt; MemAvailKb = ma
                      DiskTotalBytes = dt; DiskAvailBytes = da
                      RxBytes = rx; TxBytes = tx
                      Alerts = alerts }

                state.Set(sample)

                do! Task.Delay(TimeSpan.FromSeconds(2.0), stoppingToken)
        }
// ---------- Web app ----------
let builder = WebApplication.CreateBuilder()
builder.Services.AddSingleton<TelemetryState>() |> ignore

// Choose your interface: usually "eth0" or "wlan0"
let iface =
    match Environment.GetEnvironmentVariable("TELEMETRY_IFACE") with
    | null | "" -> "eth0"
    | s when String.IsNullOrWhiteSpace(s) -> "eth0"
    | s -> s

builder.Services.AddHostedService<Sampler>(fun sp ->
    new Sampler(sp.GetRequiredService<TelemetryState>(), iface)
) |> ignore

let app = builder.Build()

app.MapGet("/", Func<IResult>(fun () ->
    Results.Text("Pi Telemetry Agent running. Try /health or /stats")
)) |> ignore

app.MapGet("/health", Func<TelemetryState, IResult>(fun state ->
    match state.Get() with
    | None -> Results.Json({| ok = false; reason = "No sample yet" |})
    | Some s ->
        let ok = s.Alerts.IsEmpty
        Results.Json({| ok = ok; timeUtc = s.TimestampUtc; alerts = s.Alerts |})
)) |> ignore

app.MapGet("/stats", Func<TelemetryState, IResult>(fun state ->
    match state.Get() with
    | None -> Results.Problem("No sample yet")
    | Some s -> Results.Json(s)
)) |> ignore

// Prometheus-ish text (simple)
app.MapGet("/metrics", Func<TelemetryState, IResult>(fun state ->
    match state.Get() with
    | None -> Results.Text("pi_telemetry_up 0\n", "text/plain")
    | Some s ->
        let lines =
            [ "pi_telemetry_up 1"
              match s.CpuTempC with Some t -> sprintf "pi_cpu_temp_c %f" t | None -> "pi_cpu_temp_c NaN"
              match s.Load1 with Some v -> sprintf "pi_load1 %f" v | None -> "pi_load1 NaN"
              match s.MemTotalKb, s.MemAvailKb with
              | Some t, Some a -> sprintf "pi_mem_used_pct %f" (100.0 * (1.0 - float a / float t))
              | _ -> "pi_mem_used_pct NaN"
              match s.DiskTotalBytes, s.DiskAvailBytes with
              | Some t, Some a -> sprintf "pi_disk_used_pct %f" (100.0 * (1.0 - float a / float t))
              | _ -> "pi_disk_used_pct NaN" ]
        Results.Text(String.Join("\n", lines) + "\n", "text/plain")
)) |> ignore

app.Run()
