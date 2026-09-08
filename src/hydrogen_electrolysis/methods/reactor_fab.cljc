(ns hydrogen-electrolysis.methods.reactor-fab
  "hydrogen reactor fabrication cell — activity → decision → effect → audit contract.

  Scope: the :hydrogen-reactor-fabrication manufacturing cell of the
  magnesium-hydrogen-PEMFC electric-drive system (system-scope.edn). Reactor
  control and cartridge integration are RETAINED in house; MgH2 synthesis is
  OUTSOURCED (first-generation boundary) — this contract never models synthesis.

  Invariants (same policy as the igata magnesium-HPDC contract):
  - Every decision is pure. The bot may design and simulate; it may never
    command physical equipment (`:command-equipment` / `:pressurize-live-h2`
    effects are refused unconditionally, even with human approval).
  - Hazardous activity (:pressurize :admit-h2 :leak-test-live) requires an
    explicit human approval scoped to exactly that activity set.
  - Design pressure, leak-rate limits, and temperatures must be MEASURED
    inputs from the design record. This contract never invents, defaults, or
    bounds-checks against literature constants it does not carry.
  - Procurement defers to a human; condition is recorded as one of
    #{:new :used :refurbished :unknown}; missing commercial values are
    recorded as :unmeasured, never estimated."
  (:require [kotoba.lang.text :as str]))

;; ---------------------------------------------------------------------------
;; Shared refusal / approval helpers
;; ---------------------------------------------------------------------------

(def ^:private hazardous-activities
  "Cell activities that may only proceed with explicit human approval."
  #{:pressurize :admit-h2 :leak-test-live})

(def ^:private forbidden-effects
  "Effects this contract refuses unconditionally. Design/simulation only."
  #{:command-equipment :pressurize-live-h2 :admit-live-h2})

(defn- refused?
  "True when the requested effect set touches a forbidden effect."
  [requested-effects]
  (some forbidden-effects requested-effects))

(defn- approval-covers?
  [approvals activities]
  (and (set? approvals)
       (seq activities)
       (every? approvals activities)))

(defn- audit-tail
  [decision]
  (assoc decision
         :audit/cell :hydrogen-reactor-fabrication
         :audit/bot-commanded-equipment false
         :audit/refused-effects (if (refused? (get decision :request/effects #{}))
                                  (vec (filter forbidden-effects (get decision :request/effects)))
                                  [])))

;; ---------------------------------------------------------------------------
;; Decision 1 — plan reactor leak & pressure test
;; ---------------------------------------------------------------------------

(def ^:private required-test-interlocks
  "Interlocks required for the leak/pressure test gate (defined before the
  decisions that read it)."
  #{:h2-detector-armed
    :inert-purge-procedure
    :ventilation-confirmed
    :ignition-sources-cleared
    :remote-test-area
    :pressure-relief-fitted
    :barricade-exclusion-zone})

(defn required-interlocks
  "Set of interlocks required for the leak/pressure test gate. Exposure as a
  function so decisions and tests read the same source."
  []
  required-test-interlocks)

(defn plan-leak-and-pressure-test
  "Given a request map for the reactor leak/pressure test gate, return a
  decision map {:decision ... :effect ... :audit ...}.

  request keys:
    :activity           one of :plan-leak-and-pressure-test (anything else
                        yields :decision/refused)
    :request/effects    set of requested effects
    :approvals          set of human-approved activities (may be empty)
    :design-pressure-bar  MEASURED from the design record (number or nil)
    :test-medium        :helium | :nitrogen | :hydrogen
    :interlocks-present set of interlocks (see required-interlocks)

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused with
      :effect/refuse, regardless of approvals (design/simulate only).
    - Hydrogen as test medium for the leak gate is refused: leak screening is
      performed with inert gas (:helium or :nitrogen); live-hydrogen leak
      verification is :leak-test-live and needs approval AND a design-licensed
      procedure that does not exist yet (recorded :unmeasured).
    - design-pressure-bar must be a positive measured number; nil/zero/non-number
      => :decision/blocked (:blocked/unmeasured-design-pressure).
    - All required interlocks must be present, else :decision/blocked.
    - :pressurize with inert gas still needs human approval (:pressurize is
      hazardous)."
  [{:keys [activity request/effects approvals design-pressure-bar
           test-medium interlocks-present]
    :or {effects #{} approvals #{} interlocks-present #{}}
    :as request}]
  (let [request (assoc request :request/effects effects)]
    (cond
      (not= activity :plan-leak-and-pressure-test)
      (audit-tail {:decision :decision/refused
                   :reason :reason/not-this-gate
                   :effect :effect/none})

      (refused? effects)
      (audit-tail {:decision :decision/refused
                   :reason :reason/bot-may-not-command-equipment
                   :effect :effect/refuse
                   :request/effects effects})

      (= test-medium :hydrogen)
      (audit-tail {:decision :decision/refused
                   :reason :reason/leak-screening-is-inert-gas-only
                   :effect :effect/none
                   :note "live-hydrogen leak verification requires a design-licensed procedure (:unmeasured)"})

      (not (and (number? design-pressure-bar) (pos? design-pressure-bar)))
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-design-pressure
                   :effect :effect/none
                   :unmeasured [:design-pressure-bar]})

      (not-every? interlocks-present required-test-interlocks)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/missing-interlocks
                   :effect :effect/none
                   :missing-interlocks (vec (remove interlocks-present required-test-interlocks))})

      :else
      (let [needs-approval (cond-> #{:pressurize}
                             (= test-medium :helium) (conj :leak-test-live))]
        (if (approval-covers? approvals needs-approval)
          (audit-tail {:decision :decision/approved
                       :effect :effect/simulate-test-plan
                       :test-medium test-medium
                       :design-pressure-bar design-pressure-bar
                       :human-approvals-used needs-approval})
          (audit-tail {:decision :decision/waiting-human-approval
                       :effect :effect/none
                       :approval-required needs-approval
                       :approvals-provided approvals}))))))

;; ---------------------------------------------------------------------------
;; Decision 2 — screen a reactor test-equipment offer (procurement gate)
;; ---------------------------------------------------------------------------

(defn screen-equipment-offer
  "Given an offer map for the hydrogen-reactor-fabrication cell (e.g. helium
  leak detector, pressure test rig), return a procurement decision.

  offer keys:
    :offer/name        string
    :source-url        first-party (manufacturer or owner-operated dealer) URL
    :condition         :new | :used | :refurbished | :unknown
    :price :currency   as observed, or nil when unlisted
    :lead-time :utility :safety :compliance  observed or nil

  Policy:
    - Condition must be one of the four; anything else is rejected as
      malformed (never normalized to :unknown).
    - Missing price/lead-time/utility/safety/compliance are recorded
      :unmeasured — never estimated.
    - :decision/refer-to-human always: the bot never approves procurement."
  [{:keys [offer/name source-url condition price currency
           lead-time utility safety compliance]
    :as offer}]
  (let [known-conditions #{:new :used :refurbished :unknown}]
    (cond
      (not (and (string? name) (seq (str/trim name))))
      {:decision :decision/refused
       :reason :blocked/malformed-offer
       :effect :effect/none
       :audit/cell :hydrogen-reactor-fabrication
       :audit/bot-commanded-equipment false}

      (not (and (string? source-url) (str/starts-with? source-url "https://")))
      {:decision :decision/refused
       :reason :blocked/missing-first-party-source
       :effect :effect/none
       :offer/name name
       :audit/cell :hydrogen-reactor-fabrication
       :audit/bot-commanded-equipment false}

      (not (contains? known-conditions condition))
      {:decision :decision/refused
       :reason :blocked/malformed-offer
       :reason-detail "condition-not-in-#{new-used-refurbished-unknown}"
       :effect :effect/none
       :offer/name name
       :audit/cell :hydrogen-reactor-fabrication
       :audit/bot-commanded-equipment false}

      :else
      {:decision :decision/refer-to-human
       :effect :effect/none
       :offer/name name
       :condition condition
       :source-url source-url
       :cost-inputs {:price (or price :unmeasured)
                     :currency (or currency :unmeasured)
                     :lead-time (or lead-time :unmeasured)
                     :utility (or utility :unmeasured)
                     :safety (or safety :unmeasured)
                     :compliance (or compliance :unmeasured)}
       :audit/cell :hydrogen-reactor-fabrication
       :audit/bot-commanded-equipment false
       :audit/human-approval-required true})))
