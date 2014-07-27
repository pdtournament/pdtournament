Program Equilibrium Prisoner's Dilemma Tournament
-------------------------------------------------

**NOTE: This repo is a work-in-progress for now, and the tournament is not yet
open. Please check back soon. Issues/pull requests are always appreciated.**

This repo contains the source code for a "program equilibrium" iterated
prisoner's dilemma tournament that will be open to the public in the near
future. In this tournament, competitors will submit Haskell bots that play the
iterated prisoner's dilemma against other bots. Unlike the standard iterated
prisoner's dilemma, bot are allowed to view the outcomes of previous rounds of
the prisoner's dilemma and they also have the ability to run the bot they are
playing against, effectively granting them the ability to perfectly simulate
their opponent before making a move.

## Submission rules ##

The tournament will consist of each user-submitted bot playing one match
against all other bots and against itself, where a match consists of 100 rounds
of the prisoner's dilemma.

Your submission must be a Haskell function of type `Bot`. You may include
separate helper functions if necessary. The bot should output either
`Cooperate` or `Defect` in 2 seconds, otherwise `Defect` will be automatically
chosen. See the tutorial below and the `Tournament.hs` and `Bots.hs` files for
details.

You **are not** allowed to use:
* Third-party Haskell libraries (nothing outside of `base`, please)
* GHC pragma, with the sole exception of `RankNTypes`
* Anything that violates the spirit of the rules (I reserve the right to reject
  submissions; feel free to ask me about anything questionable)

Email all submissions and/or questions about the rules to
[pdtournament@gmail.com](mailto:pdtournament@gmail.com). Submissions must be
sent in by **[date TBD]**.

## Tutorial: The program equilibrium iterated prisoner's dilemma ##

The [prisoner's dilemma](https://en.wikipedia.org/wiki/Prisoner%27s_dilemma) is
a two-player game in which players can choose to either "Cooperate" or "Defect"
against their opponent.  The payoff matrix for the prisoner's dilemma is as
follows:

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
and players can use knowledge of the previous rounds to change their
strategies.

In the [program equilibrium iterated prisoner's
dilemma](http://lesswrong.com/lw/hmx/prisoners_dilemma_with_visible_source_code/),
bots are also allowed to perfectly simulate their opponent before making a
move. This allows for significantly more complex strategies in which bots can
simulate their opponents to determine if it is safe to cooperate with them, as
well as attempt to exploit less sophisticated players.

### Haskell representation ###

We can represent this game in Haskell using algebraic data types. We represent
the decision to cooperate or defect using the following type:

```haskell
data Choice = Cooperate | Defect
```

The outcome of a round is simply a pair of decisions:

```haskell
type Moves = (Choice, Choice)
```

The outcome of multiple rounds is simply a value of type `[Moves]`.

Defining a bot is slightly more complicated. In general, a bot is a value of
type `Bot`, which is a function of type `Bot -> [Moves] -> Choice`. In other
words, a bot is a function that has access to its opponent (which is also a
function of type `Bot`) and the outcomes of the rounds that have already been
played, and it must output either `Cooperate` or `Defect`.

However, because Haskell is a pure language, this type only allows us to create
determinstic bots with static strategies. In order to allow bots to choose
non-deterministically, we can define an environment that allows bots to safely
simulate other bots and perform some simple I/O actions, such as generating
random numbers, while prohibiting all other side effects. This environment is
represented by the typeclass `(Monad m) => BotEnvironment m`, which defines the
actions that can be performed in the environment, and by its instance
`BotEnvironment IO`, which defines functions bots can call to perform these
actions. See the `Tournaments.hs` file if you are interested in the details.

With this in mind, we can define a bot using a recursive type as follows:

```haskell
newtype Bot = Bot { runBot :: (BotEnvironment m) => Bot -> [Moves] -> m Choice` }
```

That is, a bot is a function of type `(BotEnvironment m) => Bot -> [Moves] -> m
Choice` inside the `Bot` data constructor.

To run the bot, we simply call the `runBot` function on the bot, the opponent,
and the history of previous round outcomes. Again, for more details, see the
`Tournament.hs` file.

## Writing a bot ##

To create a bot, we just need to write a function of type `(BotEnvironment m)
=> Bot -> [Moves] -> m Choice` and wrap it in the `Bot` data constructor.  For
example, we can write a bot that ignores both of its inputs and always
cooperates:

```haskell
cooperateBot :: Bot
cooperateBot = Bot (\_ _ -> return Cooperate)
```

Of course, this bot probably isn't going to fare very well against smarter
opponents.

The [tit-for-tat](https://en.wikipedia.org/wiki/Tit_for_tat) strategy chooses
`Cooperate` on the first round, and on all subsequent rounds it mimics the
opponent's previous move. We can implement this strategy as follows:

```haskell
titForTatBot :: Bot
titForTatBot = Bot (\_ history -> case history of
            [] -> return Cooperate
            xs -> return . snd $ last xs)
```

However, even though this strategy is very effective in the classical iterated
prisoner's dilemma, it is exploitable in the program equilibrium verion of
the game -- it is completely deterministic, so an opponent can always predict
when the tit-for-tat bot will cooperate and then defect against it. To
mitigate this, we may wish to incorporate randomness or simulate our opponent
before making our move. The `BotEnvironment` typeclass provides two useful
functions that allow us to do this:

```haskell
class (Monad m, Applicative m, Functor m) => BotEnvironment m where
    rand :: (BotEnvironment m) => m Double
    time :: (BotEnvironment m) => Int -> m a -> m (Maybe a)
```

Intuitively, `rand` generates a random `Double` between 0 and 1, which allows us to
create non-determinstic bots. For example, the following bot simply outputs
`Cooperate` or `Defect` with 50% probability.

```haskell
randomBot :: Bot
randomBot = Bot (\_ _ -> do
    choice <- rand
    return (if choice < 0.5 then Cooperate else Defect))
```

Note that we can use the `do` syntax here, as we are inside the `BotEnvironment
IO` monad.

The `time` function is similar to `System.Timeout.timeout` -- it tries to
execute an IO action in the `BotEnvironment IO` monad, returning the result
wrapped in a `Just` if the action was executed in the allotted amount of time,
and `Nothing` otherwise. `time` also returns `Nothing` if the action raises an
Exception. For example, consider the following bot:

```haskell
justiceBot :: Bot
justiceBot = Bot (\op hist -> do
    sims <- replicateM 20 . time 100000 $ runBot op cooperateBot $ invert hist
    return (if Just Defect `elem` sims || Nothing `elem` sims
                then Defect
                else Cooperate))
```

This bot simulates its opponent playing the current round against
`cooperateBot`, with a timeout of 1/10th of a second. If the opponent fails to
return `Cooperate` or `Defect` within that time, raises an exception, or
returns `Defect`, then `justiceBot` will defect. The `time` function allows us
to safely simulate an opponent that returns `undefined` or fails to terminate,
as `time` will handle the exception and return `Nothing`.

Also note that we use the `invert` function before passing the history of
previous rounds to our simulated opponent; `invert` is a helper function that
simply switches the positions of each player's moves in the list of `(Choice,
Choice)` pairs.

Using `time` and `rand`, we can build more complex bots that predict their
opponents and act non-deterministically. Check out the `Bots.hs` file for more
examples, and run the file to see an example tournament that includes the bots
discussed above.
