type t = { ic : in_channel; oc : out_channel }

let make ~ic ~oc () = { ic; oc }

let input_request { ic; _ } =
  let rec input_lines ic =
    match In_channel.input_line ic with
    | None | Some "\r" -> []
    | Some line -> line :: input_lines ic
  in

  input_lines ic |> Http_request.of_lines

let with_http_serve f =
  let handler ic oc =
    let connection = make ~oc ~ic () in
    let request = input_request connection in

    f connection request
  in
  let sockaddr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", 8080)) in
  Unix.establish_server handler sockaddr

let output_html { oc; _ } html_contents =
  Printf.fprintf oc
    "HTTP/1.1 200 OK\n\rContent-Type: text/html\n\rContent-Length: %d\n\r\n\r%s"
    (String.length html_contents)
    html_contents
