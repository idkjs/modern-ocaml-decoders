type role = Admin | User

type user =
  { name : string
  ; roles : role list
  }
module My_decoders(D : Decoders.Decode.S) = struct
  open D

  let role : role decoder =
    string >>= function
    | "ADMIN" -> succeed Admin
    | "USER" -> succeed User
    | _ -> fail "Expected a role"

  let user : user decoder =
    let* name = field "name" string in
    let* roles = field "roles" (list role) in
    succeed { name; roles }
end

module My_yojson_decoders = My_decoders(Decoders_yojson.Basic.Decode)

open My_yojson_decoders;;
D.decode_string role {| "USER" |};;
