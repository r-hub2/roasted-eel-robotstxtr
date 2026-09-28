# print method summarizes and previews engine decisions

    Code
      print(plural)
    Output
      <robots_engine_decisions_v1 [2026-08-25.1]>: 2 results, 1 evidence source
       input_id                         url robots_policy_ruleset matcher_backend
              1       https://example.com/a                google          google
              2 https://example.com/private                google          google
       policy_status matcher_status url_decision        reason
           evaluated      evaluated        allow default_allow
           evaluated      evaluated     disallow rule_disallow

---

    Code
      print(single)
    Output
      <robots_engine_decisions_v1 [2026-08-25.1]>: 1 result, 1 evidence source
       input_id                         url robots_policy_ruleset matcher_backend
              1 https://example.com/private                google          google
       policy_status matcher_status url_decision        reason
           evaluated      evaluated     disallow rule_disallow

---

    Code
      print(empty)
    Output
      <robots_engine_decisions_v1 [2026-08-25.1]>: 0 results, 1 evidence source

