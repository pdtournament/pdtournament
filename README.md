## Program Equilibrium Iterated Prisoner's Dilemma Tournament ##

**The tournament is now closed; see [this
post](http://lesswrong.com/r/discussion/lw/l16/2014_iterated_prisoners_dilemma_tournament_results/)
for the results!**

Welcome to the 2014 program equilibrium iterated PD tournament!

Anyone can submit a Haskell bot to the tournament and compete against other
user-submitted bots. Unlike the standard iterated PD, bots are not only allowed
to remember the outcomes of previous rounds but also have the ability to run the
bot they are playing against, so that each bot can perfectly simulate its
opponent before making a move. This completely changes the nature of the game
and enables [all sorts of new and interesting
strategies](http://lesswrong.com/lw/hmx/prisoners_dilemma_with_visible_source_code/).

See the tutorial below and the `Tournament.hs` and `Bots.hs` files for more
instructions on how to create and submit a bot. After the tournament is over, I will
announce the winner and make all of the code and data publically available.

## Submitting a bot ##

Email all submissions and/or questions to
[pdtournament@gmail.com](mailto:pdtournament@gmail.com). Submissions must be
sent in by **September 15, 2014 13:59 UTC**.

Submission rules:
* One submission per person.
* Your submission must be a Haskell (.hs) file that imports `Tournament.hs` and
  `Bot.hs` and contains a function of type `Bot`, as defined in
  `Tournament.hs`. See the tutorial section below for more details.
* On each turn, the bot must output either `Cooperate` or `Defect` within 5
  seconds, otherwise `Defect` will be automatically chosen. Throwing an
  Exception (for any reason) will also be treated as a `Defect`.
* The tournament will be round-robin elimination: Each bot will play one match
  against all other bots, where a match consists of 100 rounds of the
  prisoner's dilemma. At the end of the round-robin round, the lower-scoring
  half of the tournament pool will be eliminated. This process will be repeated
  until only one bot remains, or there is a tie. The whole tournament will be
  run 1000 times, and the bot that places first most frequently will be
  declated the overall winner. The payoff matrix is shown below.
* You can import and use anything from the Haskell 2010 standard libraries
  (i.e. [base](http://hackage.haskell.org/package/base)) with the exception of
  the unsafe modules and the FFI. You are also encouraged to use the functions
  and example bots in `Tournaments.hs` and `Bots.hs` as building blocks.
* Your submission file may include separate helper functions or any other style
  of code organization you desire (including helper functions representing
  other kinds of bots), as long as I can find the bot you want to enter in the
  tournament.
* User-submitted code will be made public after the tournament unless you
  request that your code remain private. Names and emails always will be kept
  private, so give your bot an interesting name!

You **are not** allowed to use:
* Third-party Haskell libraries, with the exception of this repo
* Unsafe Haskell, e.g. `System.IO.Unsafe`, `Unsafe.Coerce`, or anything similar
* The C/C++ FFI
* GHC pragma, with the sole exception of `RankNTypes`
* Anything else that violates the spirit of the rules -- I reserve the right to
  reject submissions; feel free to ask me about anything that feels
  questionable.

Happy hacking!

## Tutorial ##

The [prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner%27s_dilemma) is
a two-player game in which players can choose to either "Cooperate" or "Defect"
against their opponent.  The payoff matrix for the prisoner's dilemma looks
like this:

```
                               Player 2
                           C              D

                     ------------------------------
                    |              |               |
                C   |    (3, 3)    |    (0, 5)     |
                    |              |               |
    Player 1        |------------------------------|
                    |              |               |
                D   |    (5, 0)    |    (1, 1)     |
                    |              |               |
                     ------------------------------

```

In the iterated prisoner's dilemma, multiple rounds are played in succession,
and players can use their knowledge of the previous rounds when making a
decision.

In the [program equilibrium iterated prisoner's
dilemma](http://lesswrong.com/lw/hmx/prisoners_dilemma_with_visible_source_code/),
bots are also allowed to run their opponent and examine the output before
making a move. This allows for significantly more complex strategies in which
bots can simulate their opponents to determine if it is safe to cooperate with
them, as well as attempt to exploit less sophisticated players.

### Haskell representation ###

We can represent this game in Haskell using algebraic data types. The decision
to `Cooperate` or `Defect` forms the `Choice` type:

```haskell
data Choice = Cooperate | Defect
```

The outcome of a round is a pair of decisions, one for each player:

```haskell
type Moves = (Choice, Choice)
```

The outcome of multiple rounds is simply a list of these outcome pairs, i.e. a
value of type `[Moves]`.

Defining a bot in the program equilibrium PD is slightly more complicated. In
general, a value of type `Bot` is a function of type `Bot -> [Moves] ->
Choice`. In other words, a bot is a function that takes its opponent (which is
also a value of type `Bot`) and the outcomes of the rounds that have already
been played as input, and it must output either `Cooperate` or `Defect`.

However, because Haskell is a pure language, this type only allows us to create
determinstic bots with fixed strategies. If we want to create bots that make
decisions non-deterministically, we need to define an environment that allows
bots to safely simulate other bots and perform some simple I/O actions. Within
this environment, bots can perform certain I/O actions such as generating
random numbers and safely simulating opponents, but all other side effects are
prohibited. This environment is represented by the typeclass `(Monad m) =>
BotEnvironment m`, which defines the actions that can be performed in the
environment, as well as by its instance `BotEnvironment IO`, which contains the
actual implementations of the functions that bots can use to perform these
actions. (See the `Tournament.hs` file if you are interested in the
implementation details).

With this in mind, we can define a `Bot` using the following type:

```haskell
newtype Bot = Bot { runBot :: (BotEnvironment m) => Bot -> [Moves] -> m Choice }
```

That is, a bot is a function of type `(BotEnvironment m) => Bot -> [Moves] -> m
Choice` inside a `Bot` data constructor. To run the bot against an opponent, we
simply call the `runBot` function and pass in the bot, the opponent, and the
history of previous rounds. Note that the only way to run a bot is via the
`runBot` function, meaning that a bot cannot distinguish between when it is
being simulated by another bot and when it is being run by the tournament code.
For more details, see the `Tournament.hs` file.

### Writing a bot ###

To create a bot, we just need to write a function that fits our type constraint
`(BotEnvironment m) => Bot -> [Moves] -> m Choice` and then wrap it in the
`Bot` data constructor.  For example, we can easily write a bot that ignores
both its opponent and the history of previous rounds and always cooperates:

```haskell
cooperateBot :: Bot
cooperateBot = Bot (\_ _ -> return Cooperate)
```

Of course, this bot probably isn't going to fare very well against smarter and
more aggressive opponents.

The [tit-for-tat](https://en.wikipedia.org/wiki/Tit_for_tat) strategy chooses
`Cooperate` on the first round, and on all subsequent rounds it mimics the
opponent's previous move. We can implement this strategy as follows:

```haskell
titForTatBot :: Bot
titForTatBot = Bot (\_ history -> case history of
            [] -> return Cooperate
            xs -> return . snd $ last xs)
```

The tit-for-tat is very effective in the classical iterated prisoner's dilemma,
but it is easily exploitable in the program equilibrium verion of the game.
This is because the bot's decision is based entirely on the history of previous
moves, so an opponent can always predict when the tit-for-tat bot will
cooperate and then defect against it. Instead of acting blindly, we need to
simulate our opponent on each round before making a move, in order to determine
whether they are trustworthy enough for us to choose `Cooperate`.

This bot uses the `runBot` function to simulate its opponent playing the
current round, and then mimics whatever the opponent does in the simulation. Or
more succintly: "Do whatever my opponent is going to do."

```haskell
mirrorBot :: Bot
mirrorBot = Bot (\op hist -> runBot op mirrorBot $ invert hist)
```

We apply the `invert` function before passing the history of previous rounds to
our simulated opponent. `invert` is a helper function that simply swaps the
positions of each player's moves in a list of `(Choice, Choice)` pairs.

However, there is one major problem with this strategy: When `mirrorBot` plays
against a bot that also directly simulates its opponent, it will defect, since
the call to `runBot` will create an infinite chain of recursive function calls
that won't terminate until it is stopped by the five-second time limit. This
mutual defection could cost `mirrorBot` a significant number of points in a
tournament. `mirrorBot` would fare much better if it could simulate other bots
in a more controlled environment, so that we can run our simulations and then
make a decision based on the result. We might also want to add some randomness
to `mirrorBot`, to make its strategy harder to exploit.

To incorporate randomness and simulate other bots more safely, we need to make
use of the `IO` monad constrained by our`BotEnvironment` typeclass. The
`BotEnvironment` typeclass provides two functions:

```haskell
class (Monad m, Applicative m, Functor m) => BotEnvironment m where
    rand :: (BotEnvironment m) => m Double
    time :: (BotEnvironment m) => Int -> m a -> m (Maybe a)
```

Intuitively, `rand` generates a random `Double` between 0 and 1, which allows
us to create bots that make use of entropy. For example, the following bot
simply outputs `Cooperate` or `Defect` with 50% probability. This bot isn't
going to win any prizes for intelligence, but it is harder to simulate and
exploit compared to `cooperateBot`.

```haskell
randomBot :: Bot
randomBot = Bot (\_ _ -> do
    choice <- rand
    return (if choice < 0.5 then Cooperate else Defect))
```

The second function that the `BotEnvironment` typeclass provides is `time`,
which is similar to Haskell's built-in`System.Timeout.timeout` function. `time`
tries to strictly evaluate and then execute an I/O action, such as running a
simulation. `time` returns result wrapped in a `Just` if the action was executed
in the allotted number of microseconds and `Nothing` otherwise. `time` also
returns `Nothing` if evaluating or executing the action raises an Exception. We
can use the `time` function to beef up `mirrorBot` a bit:

```haskell
smarterMirrorBot :: Bot
smarterMirrorBot = Bot (\op hist -> do
    simulation <- time 10000 . runBot op mirrorBot $ invert hist
    return (case simulation of
                Nothing   -> Cooperate
                Just move -> move))
```

This version behaves just like the original `mirrorBot`, except that it only
waits 1/100th of a second before terminating the simulation, and outputs
`Cooperate` in the event that the simulation times out or errors out. This is
not necessarily a wise strategy, as it will always cooperate with a bot that
raises an Exception, but it at least ensures that `smarterMirrorBot` will
cooperate with bots like `mirrorBot`.

Again, note that we apply the `invert` function, so that the opponent sees the
history of moves in the correct order. It is also possible to modify the
history before passing it to the opponent, simply by adding, modifying, or
truncating the list of `[(Choice, Choice)]` pairs; this lets us observe what
the opposing bot would do under certain conditions.

We can build more complex bots that run multiple simulations using `replicateM`
and `time`:

```haskell
justiceBot :: Bot
justiceBot = Bot (\op hist -> do
    sims <- replicateM 50 . time 10000 . runBot op cooperateBot $ invert hist
    return (if Just Defect `elem` sims || Nothing `elem` sims
                then Defect
                else Cooperate))
```

This bot simulates its opponent playing the current round against
`cooperateBot` 50 times, with a timeout of 1/100th of a second per simulation.
If the opponent fails to return `Cooperate` or `Defect`, raises an exception,
or returns `Defect` in any of the simulated rounds, then `justiceBot` will
defect.

The 1/100th of a second time limit used in this example is more than enough
time for a bot that does not loop infinitely to terminate on any modern
processor; simple bots like `cooperateBot` or `titForTat` will terminate in
less than 1/1000th of a second. For the purposes of this tournament, you can
consider 1/100th of a second a safe upper bound on simulation time when writing
your own bots. In addition, the `time` function itself costs slightly less than
1/100th of a second in overhead, so you should assume you have enough time for
approximately 250 simulations at 1/100th of a second each before you get close
to the time limit (250 simulations * 2/100ths of a second = 5 seconds), or 200
to be extra safe.  These limits are deliberately vague to discourage strategies
based on precise timing and simulation-chicken, but they should give you a
sense of how many simulations you can perform.

To see these bots in action, call the `runExample` function in `Bots.hs`.
