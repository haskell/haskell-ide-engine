# Protocol

## Language Server Protocol

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md

Advantages include widespread tooling and IDE support, and standarisation.

Implemented in [LspStdio.hs](../src/Haskell/Ide/Engine/Transport/LspStdio.hs)

## Just use plain old HTTP for now

For a first pass, until we know where any performance problems actually are, the nicest thing is to do plain HTTP with as stateless as possible of a protocol. It's unlikely any initial perf issues would have anything to do with the protocol, more likely to be the result of munging the code.

## JSON

JSON has wide and popular support in a variety of languages. Supporting a uniform JSON representation will be a lot easier to manage for clients and the backend than JSON+sexprs+whatever.

## Ideas for dealing with asynchrony/blocking in clients that don't have threading/async

### Promises/continuation ids

Return a promise id and let the client decide when to block on getting the response?

`GET /type/?data=blah -> {"promise": 0}`, then `GET /promise/0` when you're ready for the answer.

How does it work if there are other async events coming too, and/or it is a long running command so there is no knowledge in the front end of when it has finished? You perform multiple requests, you got: `get -> 1, get -> 2, get -> 3`. When you want answers, those callback ids in any order you like. Point is to uniquely (but temporarily) identify an ongoing computation across the wall.

This is a bit like codata; it's a stretch, but bear with me! In total languages, if you have an indefinite stream of data, you have to keep giving control back to the caller rather than make them block indefinitely. You can either block or give them a chance to block on another continuation.


#### Improvements on this

- Let client assert a timeout. `timeout=5ms`, then if you don't have an answer within 5 ms, give them a new k id.

- Probably let client pick the id so they don't have to snip it out of the reply.
