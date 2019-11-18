type kompleksno = {re : float; im : float} (* zapisni tip *)

let nic = {re = 0.0; im = 0.0}

let imaginarna_enota = {nic with im = 1.0}

let ena = {nic with re = 1.0}

let ena_plus_i = {ena with im = 1.0}

let ena_plus_i' = {{nic with re = 1.0} with im = 1.0} (* dela ampak cudno naprej brava *)

let popravi_barvanje = function
    | _ -> None

let rec fakulteta = function
    | 0 -> 1
    | n -> n * fakulteta (n - 1)