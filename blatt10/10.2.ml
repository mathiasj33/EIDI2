module F (X: sig type t val x : t end) = struct (* Problem: Signatur spezifiziert nur x => y ist nicht mehr drin! *)
  include X (* include kopiert nur das  worauf wir Zugriff haben *)
  let f x = x :: [X.x]
end
module XY = struct type t = int let x = 1 let y = 2 end
module FXY = F (XY)

(* was wir eigentlich wollen. Dann muss ich in F aber open verwenden, damit t nur einmal definiert ist *)
module FXY = struct include XY include F (XY) end