type time = float;;
type connecting   = { when_initiated:  time; } 
type connected    = { last_ping  : (time * int) option; 
                      session_id: string; 
		    } 
type disconnected = { when_disconnected: time;  } 
 
type connection_state = 
| Connecting   of connecting * int
| Connected    of connected 
| Disconnected of disconnected 
 
type connection_info = { 
   state:  connection_state; 
   server: string; 
}


let isconnecting = Connecting ( { when_initiated = 45. }, 89)
let isconnected  = Connected  { last_ping = Some (78.,8); session_id = "id" }


let affiche_statut s = 
	match s with
	| Connecting ({ when_initiated = time }, rien) ->  "connection "^(string_of_float time)  |> print_endline
	| Connected  { last_ping = None ; session_id = s } -> "en  session :"^s |> print_endline 
	| Connected  { last_ping = Some (t,p) ; session_id = s } -> "en connexion last ping "^(string_of_float t)^" session :"^s |> print_endline 
	| Disconnected { when_disconnected = 45. } -> print_endline "tt juste"
	| Disconnected { when_disconnected = t   } ->  "déconnecté "^(string_of_float t) |> print_endline 
