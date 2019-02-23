// yakir helets 305028441 yakirh@campus.technion.ac.il
// yuval isaschar 313362097 isaschar@campus.technion.ac.il

use std::io;
use std::cmp::Ordering;
use std::env;


fn main() {
    println!("Guess the number!");

    let args: Vec<String> = env::args().collect();
    let secret_number = &args[1];

    loop {

        let secret_number: u32 = match secret_number.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };
    	
		println!("Please input your guess.");

	    let mut guess = String::new();

    	io::stdin().read_line(&mut guess)
        	.expect("Failed to read line");

    	let guess: u32 = match guess.trim().parse() {
    		Ok(num) => num,
    		Err(_) => continue,
		};

    	println!("You guessed: {}", guess);

        match guess.cmp(&secret_number) {
        	Ordering::Less => println!("Too small!"),
        	Ordering::Greater => println!("Too big!"),
        	Ordering::Equal => {
        		println!("You win!");
        		break;
        	}
    	}
    }

}