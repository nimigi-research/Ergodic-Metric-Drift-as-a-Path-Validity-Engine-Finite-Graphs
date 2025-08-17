(* ::Package:: *)

(* ErgodicMetricDrift` -- certificates for drift/minorization, mixing, and CFTP *)

BeginPackage["ErgodicMetricDrift`"];

DobrushinCoefficient::usage = "DobrushinCoefficient[P] returns α(P) = 1 - max_{x,x'} TV(P(x,·),P(x',·)) for a row-stochastic matrix P.";
TotalVariation::usage = "TotalVariation[p_, q_] = 1/2 Sum |p_i - q_i|.";
VerifyDrift::usage = "VerifyDrift[P, V, λ, b] checks E[V(X_{t+1})|x] ≤ λ V(x) + b for all states.";
FindSmallSetMinorization::usage = "FindSmallSetMinorization[P, C, m] heuristically searches ε, ν s.t. P^m(x,·) ≥ ε ν for all x∈C.";
GeometricErgodicityBounds::usage = "GeometricErgodicityBounds[P, V, λ, b, m, ε] returns (R,ρ) from drift/minorization templates.";
EntropyProduction::usage = "EntropyProduction[P, π] = Σ π(x)P(x,y) log( π(x)P(x,y) / (π(y)P(y,x)) ).";
ConductanceLowerBound::usage = "ConductanceLowerBound[P, π, S] computes Φ(S) = Q(S,S^c)/min(π(S),π(S^c)).";
CFTPMonotone::usage = "CFTPMonotone[update, min, max] returns a perfect sample for a monotone kernel with bounding chains.";
LazyCycleKernel::usage = "LazyCycleKernel[n] returns P = 1/2 I + 1/4 (left+right) on C_n.";
DirectedStarKernel::usage = "DirectedStarKernel[k, γ] returns the directed star with stickiness γ at the hub 0.";

Begin["`Private`"];

TotalVariation[p_List, q_List] := 1/2 Total@Abs[p - q];

RowTV[P_, x_, x2_] := Module[{px = P[[x]], py = P[[x2]]}, TotalVariation[px, py]];

DobrushinCoefficient[P_?MatrixQ] := Module[
  {n = Length[P], maxi = 0., tv},
  Do[
    tv = RowTV[P, i, j];
    If[tv > maxi, maxi = tv],
    {i, 1, n}, {j, 1, n}
  ];
  1 - maxi
];

VerifyStochastic[P_] := And @@ (Chop[Total[#] - 1] == 0 & /@ P) && Min[P] >= -10^-12;

VerifyDrift[P_?MatrixQ, V_List, λ_?NumericQ, b_?NumericQ] := Module[
  {EV},
  If[! VerifyStochastic[P], Return[$Failed, Module]];
  EV = P . V;
  And @@ Table[EV[[x]] <= λ V[[x]] + b + 10^-12, {x, Length[V]}]
];

(* Minorization: search via common lower envelope across C for P^m rows *)
FindSmallSetMinorization[P_?MatrixQ, C_List, m_Integer?Positive] := Module[
  {Pm = MatrixPower[P, m], n = Length[P], base, ε, ν},
  base = ConstantArray[Infinity, n];
  Do[
    base = MapThread[Min, {base, Pm[[x]]}],
    {x, C}
  ];
  ε = Total[base];
  If[ε <= 0, Return[<|"Success" -> False|>]];
  ν = base/ε;
  <|"Success" -> True, "m" -> m, "ε" -> ε, "ν" -> ν, "Certificate" -> (Pm[[#]] - ε ν & /@ C)|>
];

(* Drift/minorization → geometric bounds; constants are classical templates *)
GeometricErgodicityBounds[P_?MatrixQ, V_List, λ_?NumericQ, b_?NumericQ, m_Integer?Positive, ε_?NumericQ] := Module[
  {ρ, R},
  ρ = Max[λ, 1 - ε]; (* crude but explicit *)
  R = (b + (1 - λ) Max[V])/(1 - ρ); (* envelope constant *)
  <|"ρ" -> ρ, "R" -> R|>
];

EntropyProduction[P_?MatrixQ, π_List] := Module[
  {n = Length[P], s = 0., term},
  Do[
    If[P[[i, j]] > 0 && P[[j, i]] > 0,
      term = π[[i]] P[[i, j]] * Log[(π[[i]] P[[i, j]])/(π[[j]] P[[j, i]])];
      s += term;
    ],
    {i, n}, {j, n}
  ];
  s
];

(* Conductance through a cut S *)
ConductanceLowerBound[P_?MatrixQ, π_List, S_List] := Module[
  {Sc, Q, πS, πSc},
  Sc = Complement[Range[Length[P]], S];
  Q = Total@Table[π[[i]] P[[i, j]], {i, S}, {j, Sc}];
  πS = Total@π[[S]]; πSc = 1 - πS;
  If[πS == 0 || πSc == 0, 0., Q/Min[πS, πSc]]
];

(* Monotone CFTP template (bounding chains) *)
CFTPMonotone[update_, min_, max_] := Module[
  {t = 1, low, high, seed},
  While[True,
    seed = RandomInteger[2^t - 1]; (* deterministic replay key *)
    low = min; high = max;
    Do[
      {low, high} = With[{u = update[low, k, seed], v = update[high, k, seed]},
        {Min[u, v], Max[u, v]}
      ],
      {k, t, 1, -1}
    ];
    If[low == high, Return[low]];
    t *= 2;
  ]
];

(* Example kernels *)
LazyCycleKernel[n_Integer?Positive] := Module[{P = ConstantArray[0., {n, n}]},
  Do[
    P[[i, i]] += 1/2;
    P[[i, Mod[i, n] + 1]] += 1/4;
    P[[i, Mod[i - 2, n] + 1]] += 1/4,
    {i, n}
  ];
  P
];

DirectedStarKernel[k_Integer?Positive, γ_?NumericQ] := Module[
  {n = k + 1, P = ConstantArray[0., {k + 1, k + 1}]},
  (* hub = 1 (Mathematica indexing) *)
  P[[1, 1]] = γ; Do[P[[1, i + 1]] = (1 - γ)/k, {i, k}];
  Do[P[[i + 1, 1]] = 1., {i, k}];
  P
];

End[]; EndPackage[];