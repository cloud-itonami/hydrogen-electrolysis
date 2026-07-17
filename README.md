# hydrogen_electrolysis actor

Actor for comparing hydrogen electrolysis efficiency concepts.

Responsibilities:

- reserves the SHA-pinned Kami Engine simulator interface recorded in `repository-contracts.edn`
- ranks low-temperature water electrolysis candidates
- emits report text and kotoba datom-style records

The actor does not control a physical electrolyzer. It is a deterministic design-comparison actor.

```bash
# cljc-native (ADR-2606261200); run from the repository root:
bb test       # standalone test suite
bb analyze    # canonical out/comparison.edn + out/kotoba-datoms.edn
```

Kotoba deploy dry-run:

```bash
bb deploy-plan
```
