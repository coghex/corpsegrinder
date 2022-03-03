"use_strict";

exports.ok = OK;
exports.err_not_owner = ERR_NOT_OWNER;
exports.err_no_path = ERR_NO_PATH;
exports.err_name_exists = ERR_NAME_EXISTS;
exports.err_busy = ERR_BUSY;
exports.err_not_found = ERR_NOT_FOUND;
exports.err_not_enough_energy = ERR_NOT_ENOUGH_ENERGY;
exports.err_not_enough_resources = ERR_NOT_ENOUGH_RESOURCES;
exports.err_invalid_target = ERR_INVALID_TARGET;
exports.err_full = ERR_FULL;
exports.err_not_in_range = ERR_NOT_IN_RANGE;
exports.err_invalid_args = ERR_INVALID_ARGS;
exports.err_tired = ERR_TIRED;
exports.err_no_bodypart = ERR_NO_BODYPART;
exports.err_not_enough_extensions = ERR_NOT_ENOUGH_EXTENSIONS;
exports.err_rcl_not_enough = ERR_RCL_NOT_ENOUGH;
exports.err_gcl_not_enough = ERR_GCL_NOT_ENOUGH;

exports.pMove = MOVE;
exports.pWork = WORK;
exports.pCarry = CARRY;
exports.pAttack = ATTACK;
exports.pRangedAttack = RANGED_ATTACK;
exports.pHeal  = HEAL;
exports.pClaim = CLAIM;
exports.pTough = TOUGH;

exports.terrain_mask_wall = TERRAIN_MASK_WALL;
exports.terrain_mask_swamp = TERRAIN_MASK_SWAMP;
exports.terrain_mask_lava = TERRAIN_MASK_LAVA;

exports.color_red = COLOR_RED;
exports.color_purple = COLOR_PURPLE;
exports.color_blue = COLOR_BLUE;
exports.color_cyan = COLOR_CYAN;
exports.color_green = COLOR_GREEN;
exports.color_yellow = COLOR_YELLOW;
exports.color_orange = COLOR_ORANGE;
exports.color_brown = COLOR_BROWN;
exports.color_grey = COLOR_GREY;
exports.color_white = COLOR_WHITE;

exports.event_attack = EVENT_ATTACK;
exports.event_object_destroyed = EVENT_OBJECT_DESTROYED;
exports.event_attack_controller = EVENT_ATTACK_CONTROLLER;
exports.event_build = EVENT_BUILD;
exports.event_harvest = EVENT_HARVEST;
exports.event_heal = EVENT_HEAL;
exports.event_repair = EVENT_REPAIR;
exports.event_reserve_controller = EVENT_RESERVE_CONTROLLER
exports.event_upgrade_controller = EVENT_UPGRADE_CONTROLLER
exports.event_exit = EVENT_EXIT
exports.event_power = EVENT_POWER
exports.event_transfer = EVENT_TRANSFER;
exports.event_attack_type_melee = EVENT_ATTACK_TYPE_MELEE
exports.event_attack_type_ranged = EVENT_ATTACK_TYPE_RANGED
exports.event_attack_type_ranged_mass = EVENT_ATTACK_TYPE_RANGED_MASS
exports.event_attack_type_dismantle = EVENT_ATTACK_TYPE_DISMANTLE
exports.event_attack_type_hit_back = EVENT_ATTACK_TYPE_HIT_BACK
exports.event_attack_type_nuke = EVENT_ATTACK_TYPE_NUKE
exports.event_heal_type_melee = EVENT_HEAL_TYPE_MELEE
exports.event_heal_type_ranged = EVENT_HEAL_TYPE_RANGED

exports.find_exit_top = FIND_EXIT_TOP;
exports.find_exit_right = FIND_EXIT_RIGHT;
exports.find_exit_bottom = FIND_EXIT_BOTTOM;
exports.find_exit_left = FIND_EXIT_LEFT;
exports.find_exit = FIND_EXIT;
exports.find_creeps = FIND_CREEPS;
exports.find_my_creeps = FIND_MY_CREEPS;
exports.find_hostile_creeps = FIND_HOSTILE_CREEPS;
exports.find_sources_active = FIND_SOURCES_ACTIVE;
exports.find_sources = FIND_SOURCES;
exports.find_dropped_energy = FIND_DROPPED_ENERGY;
exports.find_dropped_resources = FIND_DROPPED_RESOURCES;
exports.find_structures = FIND_STRUCTURES;
exports.find_structures0 = FIND_STRUCTURES;
exports.find_my_structures = FIND_MY_STRUCTURES;
exports.find_hostile_structures = FIND_HOSTILE_STRUCTURES;
exports.find_flags = FIND_FLAGS;
exports.find_construction_sites = FIND_CONSTRUCTION_SITES;
exports.find_my_spawns = FIND_MY_SPAWNS;
exports.find_hostile_spawns = FIND_HOSTILE_SPAWNS;
exports.find_my_construction_sites = FIND_MY_CONSTRUCTION_SITES;
exports.find_hostile_construction_sites = FIND_HOSTILE_CONSTRUCTION_SITES;
exports.find_minerals = FIND_MINERALS;
exports.find_nukes = FIND_NUKES;

exports.structure_spawn = STRUCTURE_SPAWN;
exports.structure_extension = STRUCTURE_EXTENSION;
exports.structure_road = STRUCTURE_ROAD;
exports.structure_wall = STRUCTURE_WALL;
exports.structure_rampart = STRUCTURE_RAMPART;
exports.structure_keeper_lair = STRUCTURE_KEEPER_LAIR;
exports.structure_portal = STRUCTURE_PORTAL;
exports.structure_controller = STRUCTURE_CONTROLLER;
exports.structure_link = STRUCTURE_LINK;
exports.structure_storage = STRUCTURE_STORAGE;
exports.structure_tower = STRUCTURE_TOWER;
exports.structure_observer = STRUCTURE_OBSERVER;
exports.structure_power_bank = STRUCTURE_POWER_BANK;
exports.structure_power_spawn = STRUCTURE_POWER_SPAWN;
exports.structure_extractor = STRUCTURE_EXTRACTOR;
exports.structure_lab = STRUCTURE_LAB;
exports.structure_terminal = STRUCTURE_TERMINAL;
exports.structure_container = STRUCTURE_CONTAINER;
exports.structure_nuker = STRUCTURE_NUKER;

exports.resource_energy = RESOURCE_ENERGY;
exports.resource_power = RESOURCE_POWER;

exports.resource_hydrogen = RESOURCE_HYDROGEN;
exports.resource_oxygen = RESOURCE_OXYGEN;
exports.resource_utrium = RESOURCE_UTRIUM;
exports.resource_lemergium = RESOURCE_LEMERGIUM;
exports.resource_keanium = RESOURCE_KEANIUM;
exports.resource_zynthium = RESOURCE_ZYNTHIUM;
exports.resource_catalyst = RESOURCE_CATALYST;
exports.resource_ghodium = RESOURCE_GHODIUM;

exports.resource_hydroxide = RESOURCE_HYDROXIDE;
exports.resource_zynthium_keanite = RESOURCE_ZYNTHIUM_KEANITE;
exports.resource_utrium_lemergite = RESOURCE_UTRIUM_LEMERGITE;

exports.resource_utrium_hydride = RESOURCE_UTRIUM_HYDRIDE;
exports.resource_utrium_oxide = RESOURCE_UTRIUM_OXIDE;
exports.resource_keanium_hydride = RESOURCE_KEANIUM_HYDRIDE;
exports.resource_keanium_oxide = RESOURCE_KEANIUM_OXIDE;
exports.resource_lemergium_hydride = RESOURCE_LEMERGIUM_HYDRIDE;
exports.resource_lemergium_oxide = RESOURCE_LEMERGIUM_OXIDE;
exports.resource_zynthium_hydride = RESOURCE_ZYNTHIUM_HYDRIDE;
exports.resource_zynthium_oxide = RESOURCE_ZYNTHIUM_OXIDE;
exports.resource_ghodium_hydride = RESOURCE_GHODIUM_HYDRIDE;
exports.resource_ghodium_oxide = RESOURCE_GHODIUM_OXIDE;

exports.resource_utrium_acid = RESOURCE_UTRIUM_ACID;
exports.resource_utrium_alkalide = RESOURCE_UTRIUM_ALKALIDE;
exports.resource_keanium_acid = RESOURCE_KEANIUM_ACID;
exports.resource_keanium_alkalide = RESOURCE_KEANIUM_ALKALIDE;
exports.resource_lemergium_acid = RESOURCE_LEMERGIUM_ACID;
exports.resource_lemergium_alkalide = RESOURCE_LEMERGIUM_ALKALIDE;
exports.resource_zynthium_acid = RESOURCE_ZYNTHIUM_ACID;
exports.resource_zynthium_alkalide = RESOURCE_ZYNTHIUM_ALKALIDE;
exports.resource_ghodium_acid = RESOURCE_GHODIUM_ACID;
exports.resource_ghodium_alkalide = RESOURCE_GHODIUM_ALKALIDE;

exports.resource_catalyzed_utrium_acid = RESOURCE_CATALYZED_UTRIUM_ACID;
exports.resource_catalyzed_utrium_alkalide = RESOURCE_CATALYZED_UTRIUM_ALKALIDE;
exports.resource_catalyzed_keanium_acid = RESOURCE_CATALYZED_KEANIUM_ACID;
exports.resource_catalyzed_keanium_alkalide = RESOURCE_CATALYZED_KEANIUM_ALKALIDE;
exports.resource_catalyzed_lemergium_acid = RESOURCE_CATALYZED_LEMERGIUM_ACID;
exports.resource_catalyzed_lemergium_alkalide = RESOURCE_CATALYZED_LEMERGIUM_ALKALIDE;
exports.resource_catalyzed_zynthium_acid = RESOURCE_CATALYZED_ZYNTHIUM_ACID;
exports.resource_catalyzed_zynthium_alkalide = RESOURCE_CATALYZED_ZYNTHIUM_ALKALIDE;
exports.resource_catalyzed_ghodium_acid = RESOURCE_CATALYZED_GHODIUM_ACID;
exports.resource_catalyzed_ghodium_alkalide = RESOURCE_CATALYZED_GHODIUM_ALKALIDE;

exports.resource_mist = RESOURCE_MIST
exports.resource_biomass = RESOURCE_BIOMASS
exports.resource_metal = RESOURCE_METAL
exports.resource_silicon = RESOURCE_SILICON

exports.density_low = DENSITY_LOW
exports.density_moderate = DENSITY_MODERATE
exports.density_high = DENSITY_HIGH
exports.density_ultra = DENSITY_ULTRA

exports.power_class = POWER_CLASS
