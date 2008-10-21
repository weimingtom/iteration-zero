class Descriptive(object):
	def _description(self): return self.__doc__
	description = property(_description)

	def _name(self): return self.__class__.__name__
	name = property(_name)


class Class(Descriptive):
	"""
	Class description missing.
	"""
	calign = 0
	nalign = 0


class Barbarian(Class):
	"""
High damage dealer with 2-handed weapons.
But needs loads of hitpoints.

No magic, no gods.
	"""
	calign = +1

class Warrior(Class):
	"""
1-handed weapon user.
Can get skilled with shields.
True neutral alignment.
	"""

class Monk(Class):
	"""
Close range damage dealer. Use martial arts or
spears/staffs.

High avoidance, but limited in the weapon selection.
	"""
	calign = -1

class Mercenary(Class):
	"""
Useful as ranged attackers. Can be skilled to become
assassins by using specialiced magic.

Usually no good standing with the gods.
	"""
	calign = +1

class Wizard(Class):
	"""
Casts spells and performs rituals.
	"""
	calign = -1
	nalign = -1

class Shaman(Class):
	"""
Casts spells and performs rituals.
	"""
	nalign = +1

CLASSES = [ Barbarian(), Warrior(), Mercenary(), Monk(), Wizard(), Shaman() ]



class Race(Descriptive):
	calign = 0
	nalign = 0

	allowed_classes = []

	def _description(self): return self.__doc__
	description = property(_description)

	def _name(self): return self.__class__.__name__
	name = property(_name)
	


class Human(Race):
	"""
Humans rule (most of) the world. That's a clear
advantage to belong to this race.
	"""
	nalign = +0
	allowed_classes = [Warrior, Mercenary, Monk, Wizard]

class Troll(Race):
	"""
Beast of the nature.
Slow but good resistances.
Heals a small amount.
	"""
	nalign = +2
	allowed_classes = [Barbarian, Monk, Shaman]

class Orc(Race):
	"""
Strict tribal rules usually hide the chaotic
and wild nature of the orcs away.

Infravision.
	"""
	nalign = +0
	calign = +1
	allowed_classes = [Barbarian, Warrior, Mercenary, Shaman]

class Golem(Race):
	"""
Magical creatures.
Strong but slow, Smart but without much willpower.
	"""
	nalign = -2
	allowed_classes = [Barbarian, Wizard]

class Dryad(Race):
	"""

	"""
	nalign = +2
	allowed_classes = [Warrior, Monk, Wizard, Shaman]


class Lizard(Race):
	"""
Cunning and vicious fighers.

	"""
	nalign = +0
	allowed_classes = [Warrior, Mercenary, Monk]


RACES = [Human(),Troll(),Orc(),Golem(),Dryad(),Lizard()]