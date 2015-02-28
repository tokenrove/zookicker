
type t = float

let of_int32_ms t = (Int32.to_float t) /. 1000.
let of_float t = t
