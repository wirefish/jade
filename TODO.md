# Core

* Add aura icons.

* Selling to vendors.

* Refund karma cost when unlearning skills.

* Make unlearning a skill an offer.

* Handle light and two-handed weapons in (un)equip.

* Add more combat traits.

* When quest state changes, find just those locations on the map that need
  to be updated instead of sending the whole map.

* When an entity who reacts to quests enters/exits a location, do a partial map
  update for all avatars in map range of that location.

* Simplify quest phase initial-state: default 0, default advance amount is 1, if
  a list of symbols, state is an alist associating each with 0.

* Let NPCs have a race so they can be matched properly.

* Add intro for new player.

* Restock limited items, based on proto's original item list.

* Add chat channels.

* Add a badge to the chat pane icon when new chat appears.

* Only show quest icon if matching event is when-talk, after-give, not all
  events. Or based on attribute of entity that reacts.

* Automatically compute :offers-quests from event handler bodies? Could also
  compute :advances-quests as a list of quest and phases from which a body may
  call advance-quest?

* Unify pay-for-item and pay-for-skill and associated checks.

# Game World

* Gathering: botany (plants -> ingredients/reagents), logging (trees ->
  logs), mining (deposits -> ore), skinning (corpses -> hides), fishing.

* Pre-crafting: milling (logs -> planks), smelting (ore -> ingots), tanning
  (hides -> leather).

* Crafting: alchemy, carpentry, leatherworking, blacksmithing ->
  (weaponsmithing, armorsmithing), cooking, jewelcrafting, ...

* Three categories of magic. Druidic: healing, protection, nature/lightning
  damage. Arcane: cantrips, "magical" damage e.g. magic missile. Elemental:
  earth/air/fire/water themed protection and damage.
