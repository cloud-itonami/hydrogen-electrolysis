(ns hydrogen-electrolysis.methods.reactor-closure
  "hydrogen reactor fabrication cell — getter loading + hermetic closure
  decision contract (activity → decision → effect → audit).

  Scope: the :hydrogen-reactor-fabrication manufacturing cell of the
  magnesium-hydrogen-PEMFC electric-drive system (system-scope.edn on
  com-junkawasaki origin/main). Reactor fabrication is RETAINED in house;
  MgH2 synthesis is OUTSOURCED (first-generation boundary) — this contract
  never models synthesis. It sits BETWEEN the design record and the
  leak/pressure gate: `hydrogen-electrolysis.methods.reactor-fab` owns the
  leak/pressure verification; THIS module owns the two fabrication
  activities that precede it and that the leak gate assumes:

    activity 1 — plan-getter-loading : load the impurity getter charge named
                 by the reactor design record, inside the dry-inert glovebox
                 (pyrophoric powder: ignites on air exposure; also oxide
                 poisoning is a quality hazard)
    activity 2 — plan-hermetic-closure : close the reactor with the joint
                 method the design record specifies (hot work in a
                 hydrogen-adjacent area)

  Invariants (same policy as the reactor-fab contract in this repo):
  - Every decision is pure. The bot may design and simulate; it may never
    command physical equipment (:command-equipment / :activate-getter-live /
    :perform-closure-live effects are refused unconditionally, even with
    human approval).
  - Hazardous activity (:load-getter :perform-closure) requires an explicit
    human approval scoped to exactly that activity; absence defers, never
    approves.
  - The getter material, mass, activation temperature, atmosphere limits and
    closure temperature are MEASURED values the caller supplies from the
    design record / supplier data sheet / WPS. This contract never invents,
    defaults, or estimates any of them; missing values are :unmeasured or
    blocked — never filled in. Getter capacity is never computed or
    asserted here at all.
  - If the design record names no getter, the activity is out of scope for
    that serial (refused) — not an error to fill in.
  - Procurement defers to a human; condition is recorded as one of
    #{:new :used :refurbished :unknown}; missing commercial values are
    recorded as :unmeasured, never estimated.
  - This contract never issues a leak verdict: post-closure leak
    verification is explicitly deferred to the reactor-fab leak gate."
  (:require [clojure.set :as set]
            [kotoba.lang.text :as str]))

;; ---------------------------------------------------------------------------
;; Shared refusal / approval helpers
;; ---------------------------------------------------------------------------

(def ^:private forbidden-effects
  "Effects this contract refuses unconditionally. Design/simulation only."
  #{:command-equipment :activate-getter-live :perform-closure-live})

(def ^:private hazardous-activities
  "Cell activities that may only proceed with explicit human approval."
  #{:load-getter :perform-closure})

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
         :audit/values-invented false
         :audit/refused-effects (if (refused? (get decision :request/effects #{}))
                                  (vec (filter forbidden-effects
                                               (get decision :request/effects)))
                                  [])))

(defn- present-str? [x]
  (and (string? x) (seq (str/trim x))))

(defn- pos-num? [x]
  (and (number? x) (pos? x)))

(defn- https-url? [x]
  (and (string? x) (str/starts-with? x "https://")))

;; ---------------------------------------------------------------------------
;; Activity 1 — plan getter loading (pyrophoric powder, dry-inert glovebox)
;; ---------------------------------------------------------------------------

(def ^:private required-loading-interlocks
  "Interlocks required for the getter-loading activity: pyrophoric powder
  handled only under verified dry inert atmosphere with metal-fire
  response on hand. Defined before the decisions that read it."
  #{:dry-inert-glovebox-verified
    :o2-h2o-monitor-verified
    :no-ignition-sources
    :class-d-extinguisher
    :sealed-transfer-container-ready})

(def ^:private required-loading-interlock-set
  required-loading-interlocks)

(defn required-loading-interlocks
  "Set of interlocks required for getter loading. Exposed as a function so
  decisions and tests read the same source."
  []
  required-loading-interlocks)

(defn plan-getter-loading
  "Given a request map for the getter-loading activity, return a decision
  map {:decision ... :effect ... :plan ... :audit ...}.

  request keys:
    :reactor/serial        reactor serial this charge belongs to (MES link)
    :request/effects       set of requested effects
    :approvals             set of human-approved activities (may be empty)
    :getter/material       getter material, verbatim from the design record
    :getter/lot            supplier lot id for the charge
    :getter/data-sheet-url first-party supplier data sheet (https)
    :design-record/activation-temp-c  MEASURED from the supplier data sheet
    :design-record/getter-mass-g      MEASURED from the design record
    :procedure/atmosphere-limits      {:o2-ppm-max :h2o-ppm-max} from the
                                       handling procedure record
    :measured/atmosphere             {:o2-ppm :h2o-ppm} current glovebox
                                      readings
    :interlocks-present              set of interlocks (see
                                      required-loading-interlocks)

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused,
      regardless of approvals (design/simulate only).
    - Missing :getter/material => :decision/refused
      :reason/no-getter-in-design-record — out of scope for this serial,
      never filled in.
    - Missing serial / lot / data sheet => :decision/refused (traceability).
    - Missing activation temperature, getter mass, atmosphere limits or
      atmosphere reading => :decision/blocked with the unmeasured keys
      listed (:unmeasured), never estimated.
    - A reading above its procedure limit => :decision/blocked
      :blocked/atmosphere-out-of-limits (a real condition, not a caller
      error) listing the violated species.
    - All required interlocks must be present, else :decision/blocked.
    - :load-getter is hazardous: without explicit approval the decision is
      :decision/waiting-human-approval — absence defers, never approves."
  [{:keys [reactor/serial request/effects approvals
           getter/material getter/lot getter/data-sheet-url
           design-record/activation-temp-c design-record/getter-mass-g
           procedure/atmosphere-limits measured/atmosphere
           interlocks-present]
    :or {effects #{} approvals #{} interlocks-present #{}}
    :as request}]
  (let [request (assoc request :request/effects effects)
        limits (:o2-ppm-max atmosphere-limits)
        h2o-limit (:h2o-ppm-max atmosphere-limits)
        o2 (:o2-ppm atmosphere)
        h2o (:h2o-ppm atmosphere)
        unmeasured (cond-> []
                     (not (pos-num? activation-temp-c))
                     (conj :design-record/activation-temp-c)
                     (not (pos-num? getter-mass-g))
                     (conj :design-record/getter-mass-g)
                     (or (not (number? limits)) (not (number? h2o-limit)))
                     (conj :procedure/atmosphere-limits)
                     (or (not (number? o2)) (not (number? h2o)))
                     (conj :measured/atmosphere))
        violated (cond-> #{}
                   (and (number? o2) (number? limits) (> o2 limits))
                   (conj :o2)
                   (and (number? h2o) (number? h2o-limit) (> h2o h2o-limit))
                   (conj :h2o))]
    (cond
      (refused? effects)
      (audit-tail {:decision :decision/refused
                   :reason :reason/bot-may-not-command-equipment
                   :effect :effect/refuse
                   :request/effects effects})

      (not (present-str? material))
      (audit-tail {:decision :decision/refused
                   :reason :reason/no-getter-in-design-record
                   :effect :effect/none
                   :note "getter is a design-record fact; absence is out of scope, never filled in"})

      (not (and (present-str? serial) (present-str? lot)))
      (audit-tail {:decision :decision/refused
                   :reason :reason/missing-traceability
                   :effect :effect/none
                   :missing #{:reactor/serial :getter/lot}})

      (not (https-url? data-sheet-url))
      (audit-tail {:decision :decision/refused
                   :reason :reason/missing-first-party-data-sheet
                   :effect :effect/none})

      (seq unmeasured)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-getter-loading-inputs
                   :effect :effect/none
                   :unmeasured (vec unmeasured)})

      (seq violated)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/atmosphere-out-of-limits
                   :effect :effect/none
                   :violated violated})

      (not-every? interlocks-present required-loading-interlock-set)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/missing-interlocks
                   :effect :effect/none
                   :missing-interlocks (vec (remove interlocks-present
                                                    required-loading-interlock-set))})

      :else
      (if (approval-covers? approvals #{:load-getter})
        (audit-tail {:decision :decision/approved
                     :effect :effect/simulate-getter-loading-plan
                     :plan {:reactor/serial serial
                            :getter/material material
                            :getter/lot lot
                            :design-record/activation-temp-c activation-temp-c
                            :design-record/getter-mass-g getter-mass-g
                            ;; capacity is a design/supplier matter; this
                            ;; contract never computes or asserts one.
                            :getter/capacity :unmeasured-by-this-contract
                            :post-loading-leak-gate :deferred-to-reactor-fab-leak-gate}
                     :human-approvals-used #{:load-getter}
                     :audit/hazard-classes [:pyrophoric-powder :dry-inert-atmosphere]})
        (audit-tail {:decision :decision/waiting-human-approval
                     :effect :effect/none
                     :approval-required #{:load-getter}
                     :approvals-provided approvals})))))

;; ---------------------------------------------------------------------------
;; Activity 2 — plan hermetic closure (hot work, hydrogen-adjacent area)
;; ---------------------------------------------------------------------------

(def ^:private required-closure-interlocks
  "Interlocks required for the hermetic-closure activity: hot work inside a
  hydrogen-adjacent area. Defined before the decisions that read it."
  #{:hot-work-permit-current
    :fire-watch-assigned
    :no-hydrogen-service-in-area
    :fume-extraction-verified
    :class-d-extinguisher
    :machine-guard-verified})

(def ^:private required-closure-interlock-set
  required-closure-interlocks)

(defn required-closure-interlocks
  "Set of interlocks required for hermetic closure. Exposed as a function
  so decisions and tests read the same source."
  []
  required-closure-interlocks)

(def ^:private recognized-joint-methods
  "Closed set of closure joint methods this contract plans. A method
  outside this set is refused — never normalized into one of these."
  #{:vacuum-brazing :laser-welding :electron-beam-welding :tig-inert-purge})

(defn plan-hermetic-closure
  "Given a request map for the hermetic-closure activity, return a decision
  map {:decision ... :effect ... :plan ... :audit ...}.

  request keys:
    :reactor/serial            reactor serial being closed (MES link)
    :request/effects           set of requested effects
    :approvals                 set of human-approved activities (may be empty)
    :closure/joint-method      one of recognized-joint-methods
    :closure/wps-record-id     welding/brazing procedure specification
                               record id (traceability of the procedure)
    :design-record/closure-temp-c  MEASURED from the WPS / design record
    :interlocks-present        set of interlocks (see
                                required-closure-interlocks)

  Policy:
    - Any requested effect in forbidden-effects => :decision/refused,
      regardless of approvals (design/simulate only).
    - Missing serial => :decision/refused (traceability).
    - A joint method outside the closed set => :decision/refused
      :reason/unrecognized-joint-method — never normalized.
    - Missing WPS record id or closure temperature => :decision/blocked
      with :unmeasured, never estimated.
    - All required interlocks must be present, else :decision/blocked.
    - :perform-closure is hazardous: without explicit approval the decision
      is :decision/waiting-human-approval — absence defers, never approves.
    - The simulate plan NEVER carries a leak verdict: post-closure leak
      verification belongs to the reactor-fab leak gate and is recorded as
      deferred."
  [{:keys [reactor/serial request/effects approvals
           closure/joint-method closure/wps-record-id
           design-record/closure-temp-c interlocks-present]
    :or {effects #{} approvals #{} interlocks-present #{}}
    :as request}]
  (let [request (assoc request :request/effects effects)
        unmeasured (cond-> []
                     (not (present-str? wps-record-id))
                     (conj :closure/wps-record-id)
                     (not (pos-num? closure-temp-c))
                     (conj :design-record/closure-temp-c))]
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

      (not (contains? recognized-joint-methods joint-method))
      (audit-tail {:decision :decision/refused
                   :reason :reason/unrecognized-joint-method
                   :effect :effect/none
                   :recognized-methods (vec (sort recognized-joint-methods))})

      (seq unmeasured)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/unmeasured-closure-inputs
                   :effect :effect/none
                   :unmeasured (vec unmeasured)})

      (not-every? interlocks-present required-closure-interlock-set)
      (audit-tail {:decision :decision/blocked
                   :reason :blocked/missing-interlocks
                   :effect :effect/none
                   :missing-interlocks (vec (remove interlocks-present
                                                    required-closure-interlock-set))})

      :else
      (if (approval-covers? approvals #{:perform-closure})
        (audit-tail {:decision :decision/approved
                     :effect :effect/simulate-closure-plan
                     :plan {:reactor/serial serial
                            :closure/joint-method joint-method
                            :closure/wps-record-id wps-record-id
                            :design-record/closure-temp-c closure-temp-c
                            :post-closure-leak-gate :deferred-to-reactor-fab-leak-gate}
                     :human-approvals-used #{:perform-closure}
                     :audit/hazard-classes [:hot-work :hydrogen-adjacent-area]})
        (audit-tail {:decision :decision/waiting-human-approval
                     :effect :effect/none
                     :approval-required #{:perform-closure}
                     :approvals-provided approvals})))))

;; ---------------------------------------------------------------------------
;; Decision 3 — screen a closure-equipment offer (procurement gate)
;; ---------------------------------------------------------------------------

(defn screen-equipment-offer
  "Given an offer map for the hydrogen-reactor-fabrication cell (e.g.
  vacuum brazing furnace, laser welder, inert-atmosphere glovebox), return
  a procurement decision.

  offer keys:
    :offer/name        string
    :source-url        first-party (manufacturer or owner-operated dealer) URL
    :condition         :new | :used | :refurbished | :unknown
    :price :currency   as observed, or nil when unlisted
    :lead-time :utility :safety :compliance  observed or nil

  Policy (same as the reactor-fab procurement gate):
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
       :audit/cell :hydrogen-reactor-fabrication
       :audit/bot-commanded-equipment false}

      (not (https-url? source-url))
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
