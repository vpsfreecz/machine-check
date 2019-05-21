machine-check
=============

Run checks against a Linux machine, export as Prometheus metrics

Usage
-----

```bash
nix build
./result/bin/machine-check
# dry run - don't try to write to /run/metrics, just dump to stdout
./result/bin/machine-check dry-run
```

Designed to be run periodically via cron,
by default `machine-check` creates `/run/metrics/machine-check.prom`
containing results of all checks.

Configuration
-------------

It is possible to configure certain checks like DNS check
by providing a configuration file via environment variable:

```bash
MCCFG=./sample.config ./result/bin/machine-check
```

Check out [config.sample](config.sample) in this repository
for a list of possible options.

Output sample
-------------

```
# HELP zpool_status_errors pool errors
# TYPE zpool_status_errors gauge
zpool_status_errors{name="tank"} 0.0
# HELP zpool_status_health pool health
# TYPE zpool_status_health gauge
zpool_status_health{name="tank"} 0.0
# HELP zpool_status_parse_success parsing successful
# TYPE zpool_status_parse_success gauge
zpool_status_parse_success 0.0
# HELP zpool_status_status_reported pool reports status if 1
# TYPE zpool_status_status_reported gauge
zpool_status_status_reported{name="tank"} 1.0
# HELP zpool_status_success exitcode
# TYPE zpool_status_success gauge
zpool_status_success 0.0
# HELP zpool_list_success exitcode
# TYPE zpool_list_success gauge
zpool_list_success 1.0
```

Current checks:
 * zpool list (capacity, health)
 * zpool status (health, status, erros)
 * birdc(6) show protocols
 * dns

Future checks:
 * ping
