module D = Decoders_yojson.Basic.Decode

type shape =
  | Square of int
  | Circle of int
  | Triangle of int * int

let square_decoder : shape D.decoder =
  D.(let+ s = field "side" int in Square s)

let circle_decoder : shape D.decoder =
  D.(let+ r = field "radius" int in Circle r)

let triangle_decoder : shape D.decoder =
  D.(
    let* b = field "base" int in
    let+ h = field "height" int in
    Triangle (b, h)
  )

let shape_decoder : shape D.decoder =
  let open D in
  let* shape = field "shape" string in
  match shape with
  | "square" -> square_decoder
  | "circle" -> circle_decoder
  | "triangle" -> triangle_decoder
  | _ -> fail "Expected a shape"


let decode_list (json_string : string) : (shape list, _) result =
  D.(decode_string (list shape_decoder) json_string);;


