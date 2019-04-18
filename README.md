machine-check
=============

Run checks against a Linux machine, export as Prometheus metrics

Usage
-----

```bash
nix build
./result/bin/machine-check
```

By default `machine-check` creates `/run/metrics/machine-check.prom`
containing results of all checks.

Current checks:
 * zpool list (capacity, health)
 * zpool status (health, status, erros)

Future checks:
 * birdc(6) status
 * ping
 * dns
