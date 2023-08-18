* Add aura icons.

* Make exits just entities? Add exit-message and entry-message.

* In describe-full for an item, use the materials.

* Fix command parsing to allow unambiguous prefixes.

* Selling to vendors.

* Have buying make an offer that shows total price, you can accept it.

* Rename cost -> price. Make it one currency. make it (quantity label).

* Skills can have a :karma attribute that is the karma cost in addition to any
  other price.

* Trainers. Try to reuse some of the vendor code? Trainer has a level that
  limits which skills he can teach from an inherited list.

* Handle light and two-handed weapons in (un)equip.

* Add more combat traits.

* Quest/vendor/trainer state on map.

* Let NPCs have a race so they can be matched properly.

* Make sure to use exported symbols as quest phase state, or preferbly keywords.
  Deal with plist/alist state.

* Add intro for new player.

* Restock limited items, based on proto's original item list.

* Add quest pane.

* Move chat pane to last. Badge it when new chat appears.

* Add chat channels.
