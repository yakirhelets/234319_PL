// yakir helets 305028441 yakirh@campus.technion.ac.il
// yuval isaschar 313362097 isaschar@campus.technion.ac.il

use std::io;

fn main() {

	let mut low = 0;
	let mut high = 1000;
	let mut mid;


	let mut line = String::new();

    io::stdin().read_line(&mut line)
        .expect("Failed to read line"); //Guess the number!

	while line.trim() != "You win!" {

		mid = low+(high-low)/2;

		if line.trim() == "Please input your guess." {
			println!("{}", mid);
		}
		else {
			if line.trim() == "Too small!" {
				low=mid+1;
			}
			else {
				if line.trim() == "Too big!" {
					high=mid-1;
				}
			}
		}
		line = String::new();
		io::stdin().read_line(&mut line)
        .expect("Failed to read line");
    }

}