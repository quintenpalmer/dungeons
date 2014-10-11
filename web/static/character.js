var getCharacter = function(name) {
    jQuery.post(
        '/rest/4.0/1.0/player',
        { 'name': name },
        function(data) { populateFields(data) },
        "json");
}

var populateFields = function(data) {
    document.getElementById('name').value = data.name;
    document.getElementById('race').value = data.race;
    document.getElementById('armor').value = data.armor;
    document.getElementById('weapons').value = data.weapons;
    document.getElementById('class_').value = data.class_;
    document.getElementById('level').value = data.level;
    document.getElementById('xp').value = data.xp;
    document.getElementById('initiative').value = data.initiative;
    for (var abilName in data.abilityScores) {
        document.getElementById('abilScore' + abilName).value = data.abilityScores[abilName];
        document.getElementById('abilMod' + abilName).value = data.abilityMods[abilName];
        document.getElementById('abilModPlus' + abilName).value = data.abilityModsPlus[abilName];
    }
    document.getElementById('health').value = data.hitPoints;
    document.getElementById('bloodied').value = data.bloodied;
    document.getElementById('surgeValue').value = data.surgeValue;
    document.getElementById('surgesPerDay').value = data.surgesPerDay;
    for (var skillName in data.skills) {
        document.getElementById(skillName).value = data.skills[skillName];
    }
    for (var defName in data.defenses) {
        document.getElementById(defName).value = data.defenses[defName];
    }
    document.getElementById('speed').value = data.speed;
    document.getElementById('passiveInsight').value = data.passiveInsight;
    document.getElementById('passivePerception').value = data.passivePerception;
    var i = 1;
    for (var featName in data.feats) {
        featIndex = "feat" + i;
        currentFeat = document.getElementById(featIndex)
        currentFeat.innerHTML = featName;
        currentFeat.setAttribute('href', 'javascript:setSelected("' + featName + '", "' + data.feats[featName] + '")');
        i += 1;
    }
    var i = 1;
    for (var magicItemName in data.magicItems) {
        magicItemIndex = "magicItem" + i;
        currentFeat = document.getElementById(magicItemIndex)
        currentFeat.innerHTML = magicItemName;
        currentFeat.setAttribute('href', 'javascript:setSelected("' + magicItemName + '", "' + data.magicItems[magicItemName] + '")');
        i += 1;
    }
}

var setSelected = function(name, desc) {
    document.getElementById('selectedTitle').innerHTML = name;
    document.getElementById('selected').innerHTML = desc;
}
