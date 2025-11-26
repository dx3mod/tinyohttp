let with_serve (~host, ~port) f =
  let handler ic oc =
    let connection = Http_connection.make ~oc ~ic () in
    let request = Http_connection.input_request connection in

    f connection request
  in
  let sockaddr = Unix.(ADDR_INET (inet_addr_of_string host, port)) in
  Unix.establish_server handler sockaddr
