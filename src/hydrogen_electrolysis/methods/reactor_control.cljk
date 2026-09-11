(ns hydrogen-electrolysis.methods.reactor-control
  "hydrogen reactor operational-control cell — activity → decision → effect →
  audit contract for the RETAINED-in-house governing layer of the controlled
  hydrogen reactor.

  Scope: the :hydrogen-reactor-control boundary of the magnesium-hydrogen-PEMFC
  electric-drive system (scripts/hermes-magnesium-systems-bots/system-scope.edn
  on com-junkawasaki origin/main). Reactor FABRICATION (leak/pressure gate,
  weld joint, getter loading + hermetic closure) is covered by the sibling
  contracts in this repo (reactor-fab / reactor-weld / reactor-closure). THIS
  module owns the OPERATIONAL side — what happens after a fabricated, closed,
  leak-screened reactor is put under load to feed the PEM stack:

    activity 1 — plan-operation : start-up pre-checks, cartridge-coupled H2
                 delivery, and pressure/flow governing during operation,
                 grounded in measured design-record and live-sensor values
    activity 2 — evaluate-leak-interlock : in-operation verdict against a
                 measured leak / ambient-H2 reading and the design-record limit
    activity 3 — plan-safe-state-termination : purge/vent/shutdown to a
                 defined safe state (simulated effect only)

  Invariants (same policy as every sibling contract in this repo):
  - Every decision is pure. The bot may design and simulate; it may never
    command physical equipment (:command-equipment / :pressurize-live-h2 /
    :vent-live-h2 / :admit-live-h2 / :activate-valve-live effects are refused
    unconditionally, even with human approval).
  - Hazardous activity (:load-cartridge :pressurize :vent :purge) requires an
    explicit human approval scoped to exactly that activity set; absence
    defers, never approves.
  - Cartridge condition is recorded as one of #{:new :used :refurbished
    :unknown}; unknown is a recorded condition, never a default.
  - Design pressure, operating pressure/flow band, and leak-rate limit are
    MEASURED inputs from the design record; live measured values come from the
    caller. This contract never invents, defaults, or bounds-checks against
    literature constants it does not carry. Missing measured values are
    recorded :unmeasured or blocked — never filled in.
  - A safe-state verdict from evaluate-leak-interlock is SIMULATED: it names
    the target state and the simulated effect; it does not actuate anything.
  - Procurement defers to a human; condition is recorded as one of #{:new
    :used :refurbished :unknown}; missing commercial values are recorded
    :unmeasured, never estimated."
  (:require [kotoba.lang.text :as str]))

;; ---------------------------------------------------------------------------
;; Shared refusal / approval helpers (mirror sibling contracts)
;; ---------------------------------------------------------------------------

(def ^:private hazardous-activities
  "Cell activities that may only proceed with explicit human approval."
  #{:load-cartridge :pressurize :vent :purge})

(def ^:private forbidden-effects
  "Effects this contract refuses unconditionally. Design/simulation only."
  #{:command-equipment :pressurize-live-h2 :vent-live-h2 :admit-live-h2
    :activate-valve-live})

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
         :audit/cell :hydrogen-reactor-control
         :audit/bot-commanded-equipment false
         :audit/refused-effects (if (refused? (get decision :request/effects #{}))
                                  (vec (filter forbidden-effects (get decision :request/effects)))
                                  [])))

(defn- present-str? [x]
  (and (string? x) (seq (str/trim x))))

(defn- pos-num? [x]
  (and (number? x) (pos? x)))

(defn- https-url? [x]
  (and (string? x) (str/starts-with? x "https://")))

;; ---------------------------------------------------------------------------
;; Activity 1 — plan reactor operation (governing H2 delivery to the PEM)
;; ---------------------------------------------------------------------------

(def ^:private required-operation-interlocks
  "Interlocks required to begin / sustain controlled hydrogen delivery:
  hydrogen detection, overpressure relief, flame arrestor, grounding/bonding,
  a clear vent path, and dry-inert cartridge handling (MgH2 is pyrophoric when
  wet). Defined before the decisions that read it."
  #{:h2-detector-calibrated
    :overpressure-relief-verified
    :flame-arrestor-fitted
    :grounding-bonding-verified
    :vent-line-cleared
    :dry-inert-cartridge-handling})

(defn required-interlocks
  "Set of interlocks required for reactor operation. Exposed as a function so
  decisions and tests read the same source."
  []
  required-operation-interlocks)

(def ^:private recognized-cartridge-conditions
  #{:new :used :refurbished :unknown})

(defn plan-operation
  "Given a request map for a reactor-operating governing plan, return a
  decision map {:decision ... :effect ... :plan ... :audit ...}.

  request keys:
    :reactor/serial           reactor serial under operation (MES link)
    :cartridge/serial         coupled cartridge serial (MES link)
    :cartridge/condition      one of #{:new :used :refurbished :unknown}
    :request/effects          set of requested effects
    :approvals                set of human-approved activities (may be empty)
    :design-record/max-operating-pressure-bar  MEASURED design ceiling
    :design-record/operating-pressure-band     {:min-bar :max-bar} MEASURED
                                                from the design record
    :measured/flow-demand-lmin    live PEM-demand hydrogen flow (measured)
    :interlocks-present       set of interlocks (see required-interlocks)

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused,
      regardless of approvals (design/simulate only).
    - Missing reactor or cartridge serial => :decision/refused
      (:reason/missing-traceability).
    - Cartridge condition outside the closed set => :decision/refused
      (:reason/unrecognized-cartridge-condition) — never normalized to
      :unknown; :unknown is only a recorded condition.
    - Missing measured design pressure / band / flow demand => :decision/blocked
      with :unmeasured, never estimated.
    - A measured flow demand above the design-record operating-pressure band
      ceiling => :decision/blocked :blocked/demand-exceeds-licensed-band (a
      real condition, not a caller error).
    - All required interlocks must be present, else :decision/blocked.
    - :load-cartridge and :pressurize are hazardous: without explicit approval
      the decision is :decision/waiting-human-approval — absence defers, never
      approves."
  [{:keys [reactor/serial request/effects approvals
           design-record/max-operating-pressure-bar
           design-record/operating-pressure-band
           measured/flow-demand-lmin interlocks-present]
    :or {effects #{} approvals #{} interlocks-present #{}
         design-record/operating-pressure-band {}}
    :as request}]
  (let [request (assoc request :request/effects effects)
        cartridge-serial (get request :cartridge/serial)
        cartridge-condition (get request :cartridge/condition)
        band-min (:min-bar operating-pressure-band)
        band-max (:max-bar operating-pressure-band)
        unmeasured (cond-> []
                     (not (pos-num? max-operating-pressure-bar))
                     (conj :design-record/max-operating-pressure-bar)
                     (not (pos-num? band-min))
                     (conj :design-record/operating-pressure-band)
                     (not (pos-num? band-max))
                     (conj :design-record/operating-pressure-band)
                     (not (number? flow-demand-lmin))
                     (conj :measured/flow-demand-lmin))]
    (cond
      (refused? effects)
      (audit-tail {:decision :decision/refused
                   :reason :reason/bot-may-not-command-equipment
                   :effect :effect/refuse
                   :request/effects effects})

      (not (and (present-str? serial) (present-str? cartridge-serial)))
      (audit-tail {:decision :decision/refused
                   :reason :reason/missing-traceability
                   :effect :effect/none
                   :missing (cond-> #{}
                              (not (present-str? serial)) (conj :reactor/serial)
                              (not (present-str? cartridge-serial)) (conj :cartridge/serial))})

      (not (contains? recognized-cartridge-conditions cartridge-condition))
      (audit-tail {:decision :decision/refused
                   :reason :reason/unrecognized-cartridge-condition
                   :effect :effect/none
                   :cartridge/condition cartridge-condition
                   :recognized-conditions (vec (sort recognized-cartridge-conditions))})

      (seq unmeasured)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-operation-inputs
                   :effect :effect/none
                   :unmeasured (vec unmeasured)})

      (and (number? flow-demand-lmin)
           (pos-num? band-max)
           (> flow-demand-lmin band-max))
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/demand-exceeds-licensed-band
                   :effect :effect/none
                   :measured/flow-demand-lmin flow-demand-lmin
                   :design-record/operating-pressure-band {:min-bar band-min
                                                           :max-bar band-max}})

      (not-every? interlocks-present required-operation-interlocks)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/missing-interlocks
                   :effect :effect/none
                   :missing-interlocks (vec (remove interlocks-present
                                                    required-operation-interlocks))})

      :else
      (if (approval-covers? approvals #{:load-cartridge :pressurize})
        (audit-tail {:decision :decision/approved
                     :effect :effect/simulate-operation-plan
                     :plan {:reactor/serial serial
                            :cartridge/serial cartridge-serial
                            :cartridge/condition cartridge-condition
                            :design-record/max-operating-pressure-bar max-operating-pressure-bar
                            :design-record/operating-pressure-band {:min-bar band-min
                                                                    :max-bar band-max}
                            :measured/flow-demand-lmin flow-demand-lmin
                            ;; the governing band ceiling is licensed by the
                            ;; design record; this contract never widens it.
                            :governing-band-max-bar band-max}
                     :human-approvals-used #{:load-cartridge :pressurize}
                     :audit/hazard-classes [:hydrogen-pressure :pyrophoric-powder-on-wet]})
        (audit-tail {:decision :decision/waiting-human-approval
                     :effect :effect/none
                     :approval-required #{:load-cartridge :pressurize}
                     :approvals-provided approvals})))))

;; ---------------------------------------------------------------------------
;; Activity 2 — evaluate in-operation leak / ambient-H2 interlock
;; ---------------------------------------------------------------------------

(defn evaluate-leak-interlock
  "Evaluate the in-operation leak interlock against MEASURED live readings and
  the design-record leak-rate limit. Returns a SIMULATED verdict — it names a
  target safe state and the simulated effect; it actuates nothing.

  request keys:
    :reactor/serial              reactor under operation (MES link)
    :design-record/max-leak-rate-unit   MEASURED limit (number; caller supplies
                                        the unit on the measured reading)
    :measured/leak-rate          {:value number :unit string} live reading
    :measured/ambient-h2-ppm     number, or nil when the detector reports offline
    :request/effects             set of requested effects

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused.
    - Missing reactor serial => :decision/refused (traceability).
    - Missing measured leak rate / ambient reading / design limit =>
      :decision/blocked with :unmeasured, never estimated.
    - A nil ambient reading (detector offline) is a HARD condition:
      :decision/safe-state-and-vent (simulated) — an unarmed H2 detector cannot
      clear operation.
    - Measured leak above the design-record limit => :decision/safe-state-and-vent
      (simulated), listing the measured and limit values.
    - Otherwise :decision/clear-to-operate (simulated)."
  [{:keys [reactor/serial design-record/max-leak-rate-unit
           measured/leak-rate measured/ambient-h2-ppm request/effects]
    :or {effects #{}}
    :as request}]
  (let [request (assoc request :request/effects effects)
        leak-value (:value leak-rate)
        leak-unit (:unit leak-rate)
        unmeasured (cond-> []
                     (not (pos-num? max-leak-rate-unit))
                     (conj :design-record/max-leak-rate-unit)
                     (not (number? leak-value))
                     (conj :measured/leak-rate))]
    (cond
      (refused? effects)
      (audit-tail {:decision :decision/refused
                   :reason :reason/bot-may-not-command-equipment
                   :effect :effect/refuse
                   :request/effects effects})

      (not (present-str? serial))
      (audit-tail {:decision :decision/refused
                   :reason :reason/missing-traceability
                   :effect :effect/none
                   :missing #{:reactor/serial}})

      (seq unmeasured)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-leak-interlock-inputs
                   :effect :effect/none
                   :unmeasured (vec unmeasured)})

      (nil? ambient-h2-ppm)
      (audit-tail {:decision :decision/safe-state-and-vent
                   :reason :reason/h2-detector-offline
                   :effect :effect/simulate-safe-state-vent
                   :reactor/serial serial
                   :target-safe-state :de-pressurized-and-vented
                   :note "an unarmed H2 detector cannot clear operation; simulated safe-state-and-vent only"})

      (and (number? leak-value) (pos-num? max-leak-rate-unit)
           (> leak-value max-leak-rate-unit))
      (audit-tail {:decision :decision/safe-state-and-vent
                   :reason :reason/leak-exceeds-design-limit
                   :effect :effect/simulate-safe-state-vent
                   :reactor/serial serial
                   :measured/leak-rate {:value leak-value :unit leak-unit}
                   :design-record/max-leak-rate-unit max-leak-rate-unit
                   :target-safe-state :de-pressurized-and-vented})

      :else
      (audit-tail {:decision :decision/clear-to-operate
                   :effect :effect/simulate-continue-operation
                   :reactor/serial serial
                   :measured/leak-rate {:value leak-value :unit leak-unit}
                   :measured/ambient-h2-ppm ambient-h2-ppm}))))

;; ---------------------------------------------------------------------------
;; Activity 3 — plan safe-state termination (purge/vent/shutdown)
;; ---------------------------------------------------------------------------

(def ^:private safe-state-interlocks
  "Interlocks required to plan a controlled safe-state termination: the vent
  path must be clear, flame arrestor in place, and grounding/bonding verified
  before any purge/vent step is planned."
  #{:vent-line-cleared :flame-arrestor-fitted :grounding-bonding-verified})

(defn plan-safe-state-termination
  "Given a request map for a safe-state termination, return a SIMULATED
  purge/vent/shutdown plan decision (no physical actuation).

  request keys:
    :reactor/serial         reactor being brought to safe state (MES link)
    :request/effects        set of requested effects
    :approvals              set of human-approved activities (may be empty)
    :interlocks-present     set of interlocks (see safe-state-interlocks)

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused.
    - Missing reactor serial => :decision/refused (traceability).
    - All safe-state interlocks must be present, else :decision/blocked.
    - :vent and :purge are hazardous: without explicit approval the decision is
      :decision/waiting-human-approval — absence defers, never approves."
  [{:keys [reactor/serial request/effects approvals interlocks-present]
    :or {effects #{} approvals #{} interlocks-present #{}}
    :as request}]
  (let [request (assoc request :request/effects effects)]
    (cond
      (refused? effects)
      (audit-tail {:decision :decision/refused
                   :reason :reason/bot-may-not-command-equipment
                   :effect :effect/refuse
                   :request/effects effects})

      (not (present-str? serial))
      (audit-tail {:decision :decision/refused
                   :reason :reason/missing-traceability
                   :effect :effect/none
                   :missing #{:reactor/serial}})

      (not-every? interlocks-present safe-state-interlocks)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/missing-interlocks
                   :effect :effect/none
                   :missing-interlocks (vec (remove interlocks-present
                                                    safe-state-interlocks))})

      :else
      (if (approval-covers? approvals #{:vent :purge})
        (audit-tail {:decision :decision/approved
                     :effect :effect/simulate-safe-state-termination
                     :plan {:reactor/serial serial
                            :sequence [:inert-purge :vent-to-atmosphere :shutdown]}
                     :human-approvals-used #{:vent :purge}
                     :audit/hazard-classes [:hydrogen-pressure :ventilation]})
        (audit-tail {:decision :decision/waiting-human-approval
                     :effect :effect/none
                     :approval-required #{:vent :purge}
                     :approvals-provided approvals})))))

;; ---------------------------------------------------------------------------
;; Decision 4 — screen a reactor-control equipment offer (procurement gate)
;; ---------------------------------------------------------------------------

(defn screen-equipment-offer
  "Given an offer map for the hydrogen-reactor-control cell (e.g. a pressure
  regulator, hydrogen mass-flow controller, h2 detector), return a procurement
  decision.

  offer keys:
    :offer/name        string
    :source-url        first-party (manufacturer or owner-operated dealer) URL
    :condition         :new | :used | :refurbished | :unknown
    :price :currency   as observed, or nil when unlisted
    :lead-time :utility :safety :compliance  observed or nil

  Policy (same as every sibling procurement gate):
    - Condition must be one of the four; anything else is rejected as
      malformed (never normalized to :unknown).
    - Missing price/lead-time/utility/safety/compliance are recorded
      :unmeasured — never estimated.
    - :decision/refer-to-human always: the bot never approves procurement."
  [{:keys [offer/name source-url condition price currency
           lead-time utility safety compliance]}]
  (let [known-conditions #{:new :used :refurbished :unknown}]
    (cond
      (not (present-str? name))
      {:decision :decision/refused
       :reason :blocked/malformed-offer
       :effect :effect/none
       :audit/cell :hydrogen-reactor-control
       :audit/bot-commanded-equipment false}

      (not (https-url? source-url))
      {:decision :decision/refused
       :reason :blocked/missing-first-party-source
       :effect :effect/none
       :offer/name name
       :audit/cell :hydrogen-reactor-control
       :audit/bot-commanded-equipment false}

      (not (contains? known-conditions condition))
      {:decision :decision/refused
       :reason :blocked/malformed-offer
       :reason-detail "condition-not-in-#{new-used-refurbished-unknown}"
       :effect :effect/none
       :offer/name name
       :audit/cell :hydrogen-reactor-control
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
       :audit/cell :hydrogen-reactor-control
       :audit/bot-commanded-equipment false
       :audit/human-approval-required true})))