# README #

This is my entry to the Machine Zone AI challenge in Novosibirsk, 2015.

It is a bot for the Planet Wars-like game.

The bot named "al-baghdadi" after the notorious leader of the Islamic State of Iraq and the Levant (ISIL).

Competition details (in Russian): http://machinezone.ru/archives/141

Starter pack: https://github.com/ai-challenge-mz/getting-started

### How do I build and start a bot instance? ###

* Install SBT: http://www.scala-sbt.org/0.13/tutorial/Setup.html
* Build a bot's executable JAR-file by invoking from the root project directory:
  >sbt clean oneJar
* Run the executable file using Java 7 or 8:
  >java -jar target/scala-2.11/al-baghdadi_2.11-0.1-one-jar.jar