(* Usage examples consistent with the paper *)

<< ErgodicMetricDrift`

(* Lazy cycle *)
n = 20; P = LazyCycleKernel[n];
α = DobrushinCoefficient[P];
Print["alpha(P) = ", N@α];

(* Simple drift with V ≡ 1 (finite case) *)
V = ConstantArray[1., n];
Print["Drift check (λ=.99, b=0): ", VerifyDrift[P, V, .99, 0.]];

(* Directed star with stickiness *)
k = 8; γ = 0.6; Ps = DirectedStarKernel[k, γ];
αs = DobrushinCoefficient[Ps];
Print["alpha(P_star) = ", N@αs];

(* Minorization on the hub *)
cert = FindSmallSetMinorization[Ps, {1}, 1];
Print[cert];

(* Geometric bounds from crude template *)
bounds = GeometricErgodicityBounds[Ps, ConstantArray[1., k + 1], .8, .2, 1, cert["ε"]];
Print[bounds];