type player_id = string
 
type terr_id = string
 
type cont_id = string
 
type territory
 
type troops = int
 
val player1 : player_id
 
val player2 : player_id
 
val player3 : player_id
 
val players : player_id list
 
type continent
 
type gameboard
 
val gameboard : gameboard
 
val venezuela : territory
 
val peru : territory
 
val brazil : territory
 
val argentina : territory
 
val south_america : continent
 
val siam : territory
 
val india : territory
 
val middle_east : territory
 
val afghanistan : territory
 
val china : territory
 
val mongolia : territory
 
val ural : territory
 
val siberia : territory
 
val yakutsk : territory
 
val irkutsk : territory
 
val kamchatka : territory
 
val japan : territory
 
val asia : continent
 
val north_africa : territory
 
val egypt : territory
 
val east_africa : territory
 
val congo : territory
 
val south_africa : territory
 
val madagascar : territory
 
val africa : continent
 
val central_america : territory
 
val eastern_us : territory
 
val quebec : territory
 
val ontario : territory
 
val western_us : territory
 
val alberta : territory
 
val alaska : territory
 
val greenland : territory
 
val northwest_territory : territory
 
val north_america : continent
 
val eastern_australia : territory
 
val western_australia : territory
 
val indonesia : territory
 
val new_guinea : territory
 
val australia : continent
 
val ukraine : territory
 
val scandinavia : territory
 
val iceland : territory
 
val great_britain : territory
 
val northern_europe : territory
 
val western_europe : territory
 
val southern_europe : territory
 
val europe : continent
 
val get_territory : string -> territory list -> territory
 
val get_continent : territory -> string
 
val get_neighbors : territory -> string list
 
val is_neighbor : string -> string -> territory list -> bool

val add_troops : gameboard -> terr_id -> troops -> gameboard

val string_gameboard : gameboard -> string

val player_terrs : gameboard -> player_id -> terr_id list

val string_player_board : string -> gameboard -> string

val initialization : gameboard -> gameboard

val set_playerorder : string list -> string
