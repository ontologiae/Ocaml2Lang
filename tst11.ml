let rec pattern_match l = 
        match l with
        | []    -> 0
        | t::q  -> 1 + pattern_match q
