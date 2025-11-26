type t = {
  code_status : int;
  headers : (string * string) list;
  body : string option;
}

let output_code_status buf = function
  | 200 -> Buffer.add_string buf "200 OK"
  | _ -> raise (Invalid_argument "unknown http code status")

let output_header_line buf (key, value) =
  Buffer.add_string buf key;
  Buffer.add_char buf ':';
  Buffer.add_string buf value;
  Buffer.add_string buf "\n\r"

let output_buffer buf t =
  Buffer.add_string buf "HTTP/1.1 ";
  output_code_status buf t.code_status;
  Buffer.add_string buf "\n\r";
  List.iter (output_header_line buf) t.headers;
  Buffer.add_string buf "\n\r\n\r";
  Option.iter (Buffer.add_string buf) t.body
