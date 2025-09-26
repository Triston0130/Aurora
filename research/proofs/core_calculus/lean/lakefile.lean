import Lake
open Lake DSL

package «AuroraCalc» where
  moreLeanArgs := #["-DautoImplicit=false"]

lean_lib «AuroraCalc» where
  roots := #[`AuroraCalc.Syntax, `AuroraCalc.Typing, `AuroraCalc.Reduction, `AuroraCalc.Soundness]

@[defaultTarget]
lean_exe «aurora-calc-check» where
  root := `Main
