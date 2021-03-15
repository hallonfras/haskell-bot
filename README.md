<div align="center">
	<img src="https://files.beppp.club/ubot3.png" width="400"/>
</div>
<div align="center">
	<b>A Discord bot for Uppsala students!</b>
</div>

## ✨ Features
 - [x] Jokes and poetry
 - [x] Weather information
 - [x] Retrieving information from canvas/studium

## 🏃 Setup
1. Clone the repository
2. Create a .env file in the repositry containing the following text: DISCORD_TOKEN= "" , replace the "" with your discord token
3. Go in to Canvas.hs and add your canvas api token to the canvas_token variable
3. Run cabal repl
4. Run the main function in Main.hs

## 📌 Running tests
1. Load the Tests module
2. Run the runTests function

NOTE: Missing dependencies may have to be installed for the program to work. To install these run the "cabal repl" command. If anything is missing it will be displayed. To install the missing dependencies write " cabal install "DEPENDENCY" ".

Cabal repl may have you end up on modules other than main, if so use ":l main" to fix this.
