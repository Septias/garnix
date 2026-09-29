qUnk has no restriction. So the declarative system can give a λ-parameter the domain ★ and upcast every argument passed to it. I kernel-checked this:

λg. {a = g {l=c}; b = g {m=c}}  :  (★ → 𝓫) → {a: 𝓫 | b: 𝓫}     -- QTyped ✓
run …                           =  "fail: clash"

This has three consequences:
- The claim "rejecting a clash is soundness, not choice" is true for unification. It is false as a statement about inference: a clash can still be declaratively typeable.
- The same trick (f : ★ → 𝓫) types all of the §1 program witnesses. So relative to the declarative system, even the "can't be fixed" cases have typings, just not principal ones.
- GeneralPrincipality's covering conjunct fails far more often than the three recorded reasons suggest.

Options, cheapest first:
- (i) State completeness only for derivations that use T-★-intro directly under a selection, which is the only place the algorithm itself introduces ★. This is honest and costs nothing in the proofs. I'd take this now.
- (ii) Restrict the declarative rule so that ★-intro is allowed only at selection results. First check whether preservation (see minimal.lean:1639) only ever needs it there.
- (iii) Solve a variable to ★ on a clash when it is never eliminated (gradual inference, in the style of Siek & Vachharajani). This is a real extension.
