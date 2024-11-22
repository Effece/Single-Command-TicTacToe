let width = 3 and height = 3 and align = 3
in
	let chr_pos = function c -> int_of_char c - int_of_char 'a'
	and int_pos = function n -> int_of_char n - int_of_char '1'
	and get_ind = function (x, y) -> y * width + x
	and gen_grid = (
		let rec looper = function
			| i when i = width * height -> ""
			| i -> "-" ^ looper (i+1)
		in looper 0)
	and gen_align player_arg = (
		let rec looper = function
			| i when i = align -> ""
			| i -> (if player_arg = 0 then "o" else "x") ^ looper (i+1)
		in looper 0)
	and n_empty n = (
		let rec looper = function
			| i when i >= n -> ""
			| i -> "-" ^ looper (i+1)
		in looper 0)
	in
		let display = function chr -> (
			let rec single_disp = function
				| i when i = width * height -> print_newline ()
				| i -> if i mod width = 0 then print_newline (); print_char chr.[i]; single_disp (i+1)
			in single_disp 0)
		and read_pos = function
			| inp when String.length inp <> 2 -> (0, 0) (** raise Invalid_input *)
			| inp -> (int_pos inp.[1], chr_pos inp.[0])
		and update grid_arg position player_arg = String.sub grid_arg 0 (get_ind position) ^ (if player_arg == 0 then "o" else "x") ^ String.sub grid_arg (get_ind position + 1) (String.length grid_arg - get_ind position - 1)
		and get_column grid_arg i = (
			let rec cell = function
				| j when j = align -> ""
				| j -> String.make 1 grid_arg.[i + j * width] ^ cell (j+1)
			in cell 0)
		and shift_right grid_arg w h = (
			let rec create_line = function
				| i when i = h -> ""
				| i -> n_empty i ^ String.sub grid_arg (i * w) w ^ n_empty (h-i-1) ^ create_line (i+1)
			in create_line 0)
		and shift_left grid_arg w h = (
			let rec create_line = function
				| i when i = h -> ""
				| i -> n_empty (h-i-1) ^ String.sub grid_arg (i * w) w ^ n_empty i ^ create_line (i+1)
			in create_line 0)
		in
			let rec detect_win_line grid_arg w h = function
				| i when i > w * h - align -> false
				| i when i > (i / w + 1) * w - align -> detect_win_line grid_arg ((i / w + 1) * w) w h
				| i -> (let test = String.sub grid_arg i align in test = gen_align 0 || test = gen_align 1) || detect_win_line grid_arg (i+1) w h
			and detect_win_column grid_arg w h = function
				| i when i > w * h - align -> false
				| i when i / w > h - align -> detect_win_column grid_arg (i mod w + 1) w h
				| i -> (let test = get_column grid_arg i in test = gen_align 0 || test = gen_align 1) || detect_win_column grid_arg (i+w) w h
			and detect_win_diag grid_arg w h = detect_win_column (shift_left grid_arg w h) (w+h) h 0 || detect_win_column (shift_right grid_arg w h) (w+h) h 0
			and is_stuck grid_arg = (
				let rec looper = function
					| i when i = width * height -> true
					| i -> grid_arg.[i] <> '-' && looper (i+1)
				in looper 0)
			in
				let rec morpion grid player = function
					| _ when is_stuck grid -> print_newline (); print_string "The game is stuck!"
					| _ when (detect_win_line grid width height 0 ||
						detect_win_column grid width height 0) (** ||
						detect_win_diag grid width height) *)
						-> print_newline (); print_string ("Player " ^ string_of_int (2-player) ^ " won!")
					| pos when grid.[get_ind (read_pos pos)] == '-' -> let new_grid = update grid (read_pos pos) player in
						display new_grid; morpion new_grid (1-player) (read_line ())
					| _ -> print_newline (); print_string "Illegal move."; display grid; morpion grid player (read_line ())
				in morpion gen_grid 0 (read_line ());;

b1