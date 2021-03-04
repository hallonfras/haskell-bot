
22/2
---
Started searching for a joke API without requirements for authorization and found one called " https://icanhazdadjoke.com/ ". I then started laborating on how to implement it efficiently so that it worked with our library as well as the other functions created by my fellow groupmate. I analyzed the first test function (created by Isaac) to see what the requirements were for a bot to produce a message. I then set out with the goal of having the APIs message converted into a single string.


23/2
---
I created a newtype called Joke as well as a corresponding instance so that the data from the joke API could be adequately interpreted for its intended purpose.

I then talked to Alex who had already managed to create a api GET function for his canvas project using JSON. We then discussed about how we could globalize this functions implementation in the program and I settled on using an almost identical function to get data from my joke API.

25/2
---
I created a newtype called Joke as well as a corresponding instance so that the data from the joke API could be adequately interpreted for its intended purpose.

I then talked to Alex who had already managed to create an API GET function for his canvas project using JSON. We then discussed how we could globalize the implementation of this function in the program and I settled on using an almost identical function to get data from my joke API.

Added a converter that turned "Maybe Joke" into a string which then allowed the joke function to be implemented into main.

The joke function was now fully working.

26/2
---
Came up with the idea of creating a similar function which instead of making jokes cited poetry. This would be slightly more complex  due to poetry being structured in such a more deliberate way aswell as each poem needing an author and title. 

I at first inteded to have jokes and poetry under the same module.

I solved the issue of structuring using a simple function that added the discord "\n" newline command after each string in the array which composed the lines of the random poem which was provided by the poetry API  " https://poetrydb.org/random ".

Other than having a data constructor which contained the author, title and lines of the poem instead of a newtype the functions were mostly similar.

I ran into a problem in the conversion from the API data (Maybe Poetry) which caused a non-exhaustive pattern preventing the bot from working.

27/2
---
Solved the issue which turned out to be me not having poetry implemented as an array in the converter function.

The poetry function was now fully working.

Split the JokePoetry module into the seperate modules "Joke.hs" and "Poetry.hs"


28/2
---
Implemented functions from the utility module in my joke and poetry modules to make the code more readable and to match it with the structure of the code as a whole. 

This caused the removal of the conversion functions due to the functions msgdata and fromMaybe streamlining this.

Merged the code and worked with Isaac and Alex to have the messages sent from my modules embedded and come with an icon.

1/3
---
Wrote specifications on all my finalized functions

2/3
---
Begun work on the report and examined old project reports.

3/3
---
Worked on the report.
