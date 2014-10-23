rm dungeons.db
sqlite3 dungeons.db "
CREATE TABLE feats (
    id          TEXT,
    name        TEXT,
    description TEXT
);

CREATE TABLE featsToCharacters (
    featId      TEXT,
    charId      INTEGER
);

CREATE TABLE magicItems (
    id          TEXT,
    name        TEXT,
    description TEXT
);

CREATE TABLE magicItemsToCharacters (
    magicItemId TEXT,
    charId      INTEGER
);

CREATE TABLE powers (
    id          TEXT,
    name        TEXT,
    description TEXT
);

CREATE TABLE powersToCharacters (
    powerId     TEXT,
    charId      INTEGER
);

CREATE TABLE skillsToCharacters (
    skillName       TEXT,
    charId          INTEGER
);

CREATE TABLE itemsToCharacters (
    name        TEXT,
    count       INTEGER,
    charId      INTEGER
);

CREATE TABLE character (
    charId      PRIMARY KEY,
    name        TEXT UNIQUE,
    level       INTEGER,
    xp          INTEGER,
    class       TEXT,
    classSpec   TEXT,
    race        TEXT,
    str         INTEGER,
    con         INTEGER,
    dex         INTEGER,
    int         INTEGER,
    wis         INTEGER,
    cha         INTEGER,
    armor       TEXT,
    weapon      TEXT
);

INSERT INTO feats VALUES ('enragedBoarForm', 'Enraged Boar Form', '+1 attack, +2 damage when charging in beast form');
INSERT INTO feats VALUES ('ritualCasting', 'Ritual Casting', 'Animal Messanger');
INSERT INTO feats VALUES ('improvedTiger', 'Improved Tiger', '+2 damage with combat advantage in beast form');
INSERT INTO feats VALUES ('primalFury', 'Primal Fury', '+1 to attacks with primal powers against bloodied enemies');

INSERT INTO magicItems VALUES ('autumnHarvestTotem', 'Autumn Harvest Totem', 'On crit +1d6 per plus or +1d10 damage per plus vs a bloodied creature. - Attacks made through this item do extra damage against bloodied creatures equal to 1 + 1/2 enchantment');
INSERT INTO magicItems VALUES ('pouncingBeastArmor', 'Pouncing Beast Armor', '+1 AC (Hide Armor) - The Armor urges you to attack. - Wild Shape: on transform you shift +1 square. - Power (Daily): Shift up to 5 squares on transformation, MUST end adjacent to enemy');
INSERT INTO magicItems VALUES ('emeraldStaff', 'Emerald Staff', 'It glows with an errie light that catches the eye. Gold elvish runes inscripted on the handle. - Major Action (Encounter Power): Can dominate animal of lesser level (Wisdom vs. Will) x 1/2 animal level');

INSERT INTO powers VALUES ('wildShape', 'Wild Shape', 'Effect: You change from your humanoid form to best form or vie versa. When you change from beast form back to munaoid form, you shift 2 square. While you are in best form you cant use attack utility of reat power that lack the best form keyword, although you can sustain such powers. - You choose a speicific form whenever you use wild shape to change into best form. The best form is your size, resembles a natural best for fey best, and normally doesnt change your game statistics or momement modes. Your equipment becomes part of your beast form, but you drop andything you holding except implements you can use. You continute to gainthe benefits of the equipment you wear. - You can use the porpertiest of the powers of implements as well as magic items that you wear, but not the properties or powers of weapons or the powers of wonderous items. While quirepment is part of your best form, it cannot be removed and anything in a container that is part of your best form is inaccessible');
INSERT INTO powers VALUES ('pounce', 'Pounce', 'At Will - Beast Form, Implement, Primal - Standard Action - Melee touch - Target: One creature - Attack: Wisdom vs Reflex - Hit: 1d8 + Wisdom modifier damage. The target grants combat advantage to the next creature that attacks it before the end of your next turn. - Special: When charging, you can use this power in place of a meleebasic attack');
INSERT INTO powers VALUES ('graspingClaws', 'Grasping Claws', 'At Will - Beast Form, Implement, Primal - Standard Action - Melee touch - Target: One creature - Attack: Wisdom vs Reflex - Hit: 1d8 + Wisdom modifier damage, and the target is slowed until the end of your next turn');
INSERT INTO powers VALUES ('flameSeed', 'Flame Seed', 'At Will - Fire, Implement, Primal, Zone - Standard Action - Ranged 10 - Target: One creature - Attack: Wisdom vs Reflex - Hit: 1d6 fire damage, and the squares adjacent to the target become a fiery zone that lasts until the end of your next turn. Any enemy that enters the zone or starts its turn there takes fire damage equal to your Wisdom modifier');
INSERT INTO powers VALUES ('cullTheHerd', 'Cull the Herd', 'Encounter - Beast Form, Charm, Implement, Primal - Standard Action - Ranged 5 - Target: One creature - Attack: Wisdom vs Will - Hit: 2d8 + Wisdom modifier psychic damage, and you pull the target 3 squares');
INSERT INTO powers VALUES ('predatorsFlurry', 'Predators Flurry', 'Encounter - Beast Form, Implement, Primal - Standard Action - Melee touch - Target: One creature - Attack: Wisdom vs Reflex - Hit: 1d6 + Wisdom modifier damage, and the primary target is dazed until the end of your turn. - Effect: You shift 2 squares and then make a secondary attack. - Primal Predator: The number of squares you shift equal to your Dexterity modifier. - Secondary Target: One creature other than the primary target - Secondary Attack: Wisdom vs. Reflex - Hit: 1d6 + Wisdom modifier damage and the target is dazed until the end of your next turn.');
INSERT INTO powers VALUES ('faerieFire', 'Faerie Fire', 'Daily - Implement, Primal, Radiant - Standard Action - Area burst 1 within 10 squares - Target: Each creature in burst - Attack: Wisdom vs Will - Hit: The target is slowed and grants combat advantage (save ends both) - Aftereffect 3d6 + Wisdom modifier radiant damage and target grants combat advantage until end of you next turn. - Miss: 1d6 + Wisdom modifier radiant damage and the target grants combat advantage until the end of your next turn');
INSERT INTO powers VALUES ('fleetPursuit', 'Fleet Pursuit', 'Daily - Beast Form, Primal - Minor Action - Effect: Until the end of the encounter, you gain a power bonus to your speed while you are in beast form equal to your Dexterity modifier.');


INSERT INTO character VALUES (
    0,
    'Prompt',
    4,
    4775,
    'druid',
    'primalPredator',
    'halfling',
    12,
    14,
    16,
    11,
    18,
    11,
    'light',
    'staff'
);

INSERT INTO featsToCharacters VALUES ('enragedBoarForm', 0);
INSERT INTO featsToCharacters VALUES ('ritualCasting', 0);
INSERT INTO featsToCharacters VALUES ('improvedTiger', 0);
INSERT INTO featsToCharacters VALUES ('primalFury', 0);

INSERT INTO magicItemsToCharacters VALUES ('autumnHarvestTotem', 0);
INSERT INTO magicItemsToCharacters VALUES ('pouncingBeastArmor', 0);
INSERT INTO magicItemsToCharacters VALUES ('emeraldStaff', 0);

INSERT INTO powersToCharacters VALUES ('wildShape', 0);
INSERT INTO powersToCharacters VALUES ('pounce', 0);
INSERT INTO powersToCharacters VALUES ('flameSeed', 0);
INSERT INTO powersToCharacters VALUES ('graspingClaws', 0);
INSERT INTO powersToCharacters VALUES ('cullTheHerd', 0);
INSERT INTO powersToCharacters VALUES ('predatorsFlurry', 0);
INSERT INTO powersToCharacters VALUES ('faerieFire', 0);
INSERT INTO powersToCharacters VALUES ('fleetPursuit', 0);

INSERT INTO skillsToCharacters VALUES ('Endurance', 0);
INSERT INTO skillsToCharacters VALUES ('Heal', 0);
INSERT INTO skillsToCharacters VALUES ('Nature', 0);
INSERT INTO skillsToCharacters VALUES ('Perception', 0);

INSERT INTO itemsToCharacters VALUES ('Amythist', 1, 0);
INSERT INTO itemsToCharacters VALUES ('Gold', 1000, 0);
INSERT INTO itemsToCharacters VALUES ('Ruby Dust', 1, 0);
INSERT INTO itemsToCharacters VALUES ('Tentacle', 1, 0);
"
