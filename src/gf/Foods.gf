abstract Foods = {
  flags startcat = Comment ;

  cat
    Comment ;
    Item ;
    Kind ;
    Quality ;

  fun
    -- | Comments are created from an Item and a Quality.
    Pred : Item -> Quality -> Comment ;

    -- | Modifiers on things (which thing?); turns a thing into an item.
    This, That, These, Those : Kind -> Item ;

    -- | Given a thing, what do you want to say about it?
    Mod : Quality -> Kind -> Kind ;

    -- | Some things:
    Wine, Cheese, Fish, Pizza : Kind ;

    -- | Optional modifier on quality.
    Very : Quality -> Quality ;

    -- | Lexicon of qualities:
    Fresh, Warm, Italian, Expensive, Delicious, Boring : Quality ;
}
