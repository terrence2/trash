
let main () = 
	let foo = Interact.interact () in
		print_endline ("You entered: " ^ foo);
		exit (4);;

main();;
