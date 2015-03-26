---
title: Reading from processes safely in Haskell
tags: haskell, functional programming, work, process, pipes, deadlock
location: Sydney, New South Wales
excerpt: 
  The naive approach to invoking a process and reading it's output is not safe.
  Here's a snippet of code which does this safely.
---

Quite a bit of my work lately has been implementing HTTP interfaces to existing
systems. In a few cases this required invoking existing command-line tools and
parsing their output. The naive approach to invoking a [process][] in Haskell
and reading its output goes something like this:

[process]: https://hackage.haskell.org/package/process

````{.haskell}
import System.Exit
import System.Process

main :: IO ()
main = do
    let p = (shell "cat /usr/share/dict/words")
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = Inherit
            }
    (Nothing, Just out, Nothing, ph) <- createProcess p
    ec <- waitForProcess ph
    case ph of
        ExitSuccess   -> hGetContents out >>= print
        ExitFailure _ -> error "Bad things happened. :-("
````

There is a potential problem in this code: we wait until the process has
terminated before reading the `Handle` allowing its output to accumulate in the
pipe buffer managed by the operating system in the mean time. This buffer has
a fixed size on most systems (this is a good thing!); when it fills up, the
writing process will go to sleep until the reader has consumed some data and
freed some buffer space to hold the next write. Alas, the reader (the Haskell
code above) is sleeping, waiting for the writer to terminate. The reader is
sleeping, waiting for the writer to terminate; and the writer is sleeping,
waiting for the reader to read. This is a [deadlock][]!

[deadlock]: https://en.wikipedia.org/wiki/Deadlock

The solution is to do the Right Thing (tm) and take care of any buffering
behaviour we want ourselves. Thankfully this is pretty straightforward and it's
the sort of code you generally only need to write once. The very simplest case
-- reading from a process with a single output `Handle` -- looks like this:

````{.haskell}
gatherOutput :: ProcessHandle -> Handle -> IO (ExitCode, ByteString)
gatherOutput ph h = work mempty
  where
    work acc = do
        -- Read any outstanding input.
        bs <- BS.hGetNonBlocking h (64 * 1024)
        let acc' = acc <> bs
        -- Check on the process.
        s <- getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work acc
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                last <- BS.hGetContents h
                return (ec, acc <> last)
````

This is essentially a loop which reads some input from the `Handle` (possibly
an empty string), checks to see if the process has terminated, and either
returns the accumulated input or loops again. Extending this to gather the
output of two handles (like `stderr` and `stdout`) is relatively
straightforward.
