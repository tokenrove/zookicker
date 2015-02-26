
type t = float

let of_int32_ms t = 1000. /. (Int32.to_float t)
let of_float t = t
