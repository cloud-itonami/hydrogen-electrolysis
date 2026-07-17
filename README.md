# hydrogen_electrolysis actor

Actor for comparing hydrogen electrolysis efficiency concepts.

Responsibilities:

- calls the Kami Engine simulation package at `40-engine/kami-engine/kami-hydrogen-electrolysis-sim`
- ranks low-temperature water electrolysis candidates
- emits report text and kotoba datom-style records

The actor does not control a physical electrolyzer. It is a deterministic design-comparison actor.

```bash
# cljc-native (ADR-2606261200); run from the actor root:
bash run_tests.sh                                    # cljc test suite (electrolysis + charter gates)
bb -cp .. -m hydrogen-electrolysis.methods.analyze   # efficiency comparison -> out/comparison.{json,md}
```

Kotoba deploy dry-run:

```bash
cd kotoba
./deploy.sh
```
