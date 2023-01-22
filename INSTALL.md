# Before anything make sure that your opam is updated
opam update

# Upgrade opam to make sure opam is up to date
opam upgrade

# Install the json and spectrum (colors on terminal) library
opam install yojson
# When installing spectrum, I ran into a problem with pkg-config. If this 
# happens to you, run "sudo apt-get install -y pkg-config"
opam install spectrum

# Compile
make compile

# Play !
make play 