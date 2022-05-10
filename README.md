# purescript-erl-bus

Various message bus implemetations built on top of Gproc (and as such that dependency needs adding to rebar.config)

## SimpleBus

Anyone can raise messages, anyone can subscribe...

### Defining a bus and message type

```purescript

    data BookEvent = BookCreated Isbn
               | BookUpdated Isbn
               | BookDeleted Isb

    bus :: SimpleBus.Bus String BookEvent
    bus = SimpleBus.bus "book_library"

```

### Sending Messages on the bus

```purescript

   SimpleBus.raise bus (BookCreated book.isbn)

```

### Subscribing to Messages

```purescript

  _ <- SimpleBus.subscribe BookLibrary.bus handleMessage

  handleMessage :: BookEvent :: Effect Unit
  handleMessage ev = pure unit

```

## MetadataBus

Only one process can publish - anyone can subscribe.

Publisher provides metadata (context) and then a sequence of messages within that context. Subscribers get the current metadata when they join the bus and then any new messages / metadata

### Defining a bus and message type

```purescript

    TBD

```

### Sending Messages on the bus

```purescript

   TBD

```

### Subscribing to Messages

```purescript

  TBD

```

## StateBus

Only one process can publish - anyone can subscribe.

Publisher provides initial state, a state update function `(state -> msg -> state)` and then raises messages (but not new state). Subscribers get the current state when they join the bus and then any messages raised thereafter.
