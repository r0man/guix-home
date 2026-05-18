# Security audit — scans wrapping the Implement and Submit phases

Use this when the code is security-sensitive: authentication/authorization,
cryptography, input parsing, file/path handling, anything touching secrets,
credentials, or untrusted/external data.

This is a cross-cutting aspect: it adds a *prescan* before and a *postscan*
after the Implement phase, and again around the Submit phase. The reason for
both sides is that security regresses in two places — when you write the code
(introducing a vulnerable pattern) and at the boundary where it merges (a
dependency or interaction that was safe in isolation becomes unsafe combined).

## Around Implement

**Prescan (before implementing):**

- Review the scope for secrets/credentials that must not be hardcoded or logged.
- Check the dependencies you're about to use for known vulnerabilities.
- Note the trust boundaries the change crosses and the inputs that will be
  attacker-controlled.

**Postscan (after implementing):**

- Scan the new code for vulnerabilities (SAST mindset): injection (SQL, command,
  path traversal, SSRF), missing input validation/sanitization, unsafe
  deserialization, insecure crypto usage.
- Check for hardcoded secrets or credentials introduced by the change.
- Review against the OWASP Top 10 for the relevant categories.

## Around Submit

**Prescan (before submitting):** final vulnerability scan of the full diff
before it lands.

**Postscan (after submitting):** confirm no new vulnerabilities were introduced
by the integration — re-check that nothing safe-in-isolation became unsafe once
combined with the rest of the branch.

If any scan surfaces a real issue, fix it before clearing the phase's gate —
don't carry a known vulnerability forward "to fix later". Later rarely comes,
and the gate exists precisely to stop that.
