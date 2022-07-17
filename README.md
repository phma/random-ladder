# Random Cryptographic Ladder
This library aims to defeat side-channel attacks on public-key encryption. A common method of doing this is to make all steps of the ladder take equal time. This doesn't work in Haskell because of laziness. Instead, Random Ladder uses a random number to make the sequence of steps unpredictable from the integer the generator is multiplied by.
