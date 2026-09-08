(ns hydrogen-electrolysis.methods.reactor-weld
  "hydrogen reactor fabrication cell — joint fabrication (welding) decision
  contract (activity → decision → effect → audit).

  Scope: the :hydrogen-reactor-fabrication manufacturing cell of the
  magnesium-hydrogen-PEMFC electric-drive system (system-scope.edn on
  com-junkawasaki origin/main). Reactor fabrication is RETAINED in house;
  MgH2 synthesis is OUTSOURCED (first-generation boundary). This module owns
  the joint-fabrication activity that precedes :plan-hermetic-closure
  (`hydrogen-electrolysis.methods.reactor-closure`) and the leak/pressure
  gate (`hydrogen-electrolysis.methods.reactor-fab`):

    activity — plan-joint-fabrication : produce the joints named by the
               reactor design record (weld / braze), each under a measured
               procedure specification, with qualified operators and a
               defined inspection set — for hydrogen-pressure service.

  Invariants (same policy as the sibling contracts in this repo):
  - Every decision is pure. The bot may design and simulate; it may never
    command physical equipment (:command-equipment / :perform-weld-live /
    :weld-live effects are refused unconditionally, even with human approval).
  - Hazardous activity (:perform-weld) requires an explicit human approval
    scoped to exactly that activity; absence defers, never approves.
  - WPS id, base-material, filler, joint geometry, inspection method and
    acceptance level are MEASURED values the caller supplies from the design
    record / WPS. This contract never invents, defaults, or estimates any of
    them; missing values are blocked (:blocked/unmeasured-*) — never filled
    in. Design pressure is never computed here.
  - A joint whose design record names no procedure is out of scope for that
    serial (refused) — not an error to fill in.
  - A hydrogen-service joint without a specified inspection set is blocked,
    not waved through."
  (:require [kotoba.lang.text :as str]))

;; ---------------------------------------------------------------------------
;; Refusal / approval helpers (mirror sibling contracts)
;; ---------------------------------------------------------------------------

(def ^:private hazardous-activities #{:perform-weld})

(def ^:private forbidden-effects
  #{:command-equipment :perform-weld-live :weld-live})

(defn- refused?
  [requested-effects]
  (some forbidden-effects requested-effects))

(defn- audit-tail
  [decision]
  (assoc decision
         :audit/cell :hydrogen-reactor-fabrication
         :audit/bot-commanded-equipment false
         :audit/refused-effects (if (refused? (get decision :request/effects #{}))
                                  (vec (filter forbidden-effects (get decision :request/effects)))
                                  [])))

(def ^:private inspection-methods
  "Inspection methods this contract accepts as records (it performs none)."
  #{:visual :dye-penetrant :radiography :ultrasonic :helium-leak})

(defn joint-requires-approval
  "Activities requiring explicit human approval for a joint-fabrication plan."
  []
  hazardous-activities)

;; ---------------------------------------------------------------------------
;; Decision — plan joint fabrication
;; ---------------------------------------------------------------------------

(defn plan-joint-fabrication
  "Given a request map for a hydrogen-service joint, return a decision map
  {:decision ... :effect ... :audit ...}.

  request keys:
    :activity              :plan-joint-fabrication (anything else yields
                           :decision/refused)
    :request/effects       set of requested effects
    :approvals             set of human-approved activities (may be empty)
    :joint/id              joint identifier from the design record
    :wps-id                procedure spec id from the design record, or nil
                           when the record names none
    :base-material         measured base material designation, or nil
    :filler                measured filler designation, or nil
    :service-pressure-bar  MEASURED design service pressure (number or nil)
    :inspection-set        collection of inspection methods from the design
                           record (subset of inspection-methods), or empty
    :operator-qualified    true when the operator holds current qualification
                           for the WPS, false, or nil when unverified

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused with
      :effect/refuse, regardless of approvals (design/simulate only).
    - No WPS named by the design record => out of scope for this joint
      (:decision/refused, :reason/no-procedure-specified) — never invented.
    - Missing base-material / service-pressure-bar / operator qualification
      => :decision/blocked with :blocked/unmeasured-* reason and the joint
      recorded as not started. A nil filler is recorded :unmeasured but does
      not block on its own (some joint types carry none) — it never defaults.
    - A hydrogen-service joint (:service-pressure-bar > 0) with an empty
      inspection set is blocked (:blocked/inspection-set-required). Unknown
      inspection methods (not in inspection-methods) are also blocked.
    - :perform-weld needs human approval; unqualified operator also blocks
      even with approval."
  [{:keys [activity request/effects approvals joint/id wps-id base-material
           filler service-pressure-bar inspection-set operator-qualified]
    :or {effects #{} approvals #{} inspection-set #{}}
    :as request}]
  (let [request (assoc request :request/effects effects)
        inspection-set (set inspection-set)]
    (cond
      (not= activity :plan-joint-fabrication)
      (audit-tail {:decision :decision/refused
                   :reason :reason/not-this-gate
                   :effect :effect/none
                   :joint/id id})

      (refused? effects)
      (audit-tail {:decision :decision/refused
                   :reason :reason/bot-may-not-command-equipment
                   :effect :effect/refuse
                   :joint/id id
                   :request/effects effects})

      (not (and (string? wps-id) (seq (str/trim wps-id))))
      (audit-tail {:decision :decision/refused
                   :reason :reason/no-procedure-specified
                   :effect :effect/none
                   :joint/id id
                   :note "design record names no procedure for this joint; out of scope, never invented"})

      (not (string? base-material))
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-base-material
                   :effect :effect/none
                   :joint/id id
                   :unmeasured [:base-material]})

      (not (and (number? service-pressure-bar) (not (neg? service-pressure-bar))))
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-service-pressure
                   :effect :effect/none
                   :joint/id id
                   :unmeasured [:service-pressure-bar]})

      (and (pos? service-pressure-bar) (empty? inspection-set))
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/inspection-set-required
                   :effect :effect/none
                   :joint/id id
                   :service-pressure-bar service-pressure-bar})

      (not-every? inspection-methods inspection-set)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unknown-inspection-method
                   :effect :effect/none
                   :joint/id id
                   :unknown-inspection (vec (remove inspection-methods inspection-set))})

      (not (true? operator-qualified))
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/operator-qualification-unverified
                   :effect :effect/none
                   :joint/id id})

      :else
      (if (contains? approvals :perform-weld)
        (audit-tail {:decision :decision/approved
                     :effect :effect/simulate-joint-plan
                     :joint/id id
                     :wps-id wps-id
                     :base-material base-material
                     :filler (or filler :unmeasured)
                     :service-pressure-bar service-pressure-bar
                     :inspection-set (vec (sort (map name inspection-set)))
                     :human-approvals-used #{:perform-weld}})
        (audit-tail {:decision :decision/waiting-human-approval
                     :effect :effect/none
                     :joint/id id
                     :approval-required hazardous-activities
                     :approvals-provided approvals})))))
