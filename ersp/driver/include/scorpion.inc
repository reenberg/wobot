/* Scorpion robot definitions:
 *
 * SCORPION_INTERFACE(player-interface, interface-name)
 * SCORPION_(robot-dev-ID, ersp-dev-type, ersp-dev-id)
 */

/* General interfaces/devices: */
SCORPION_INTERFACE(position2d, drive)

	SCORPION_(DRIVE,         DRIVE,     "drive")
	SCORPION_(AVOID,         AVOID,     "avoidance")

SCORPION_END

SCORPION_INTERFACE(power, battery)

	SCORPION_(BATTERY,       BATTERY,   "Battery")

SCORPION_END

/* Bump (digital) sensors: */
SCORPION_INTERFACE(bumper, front)

	SCORPION_(BUMP_BNW,      BUMP,      "Bump_bnw")
	SCORPION_(BUMP_BNE,      BUMP,      "Bump_bne")

SCORPION_END

SCORPION_INTERFACE(bumper, ir)

	/* Front/up IR (digital) sensors: */
	SCORPION_(IR_TN_WUP,     IR_BINARY, "IR_tn_wup")
	SCORPION_(IR_TN_NUP,     IR_BINARY, "IR_tn_nup")
	SCORPION_(IR_TN_EUP,     IR_BINARY, "IR_tn_eup")

	/* Front/side IR (digital) sensors: */
	SCORPION_(IR_TN_WDOWN,   IR_BINARY, "IR_tn_wdown")
	SCORPION_(IR_TN_EDOWN,   IR_BINARY, "IR_tn_edown")

	/* Back/side IR (digital) sensors: */
	SCORPION_(IR_TS_WDOWN,   IR_BINARY, "IR_ts_wdown")
	SCORPION_(IR_TS_EDOWN,   IR_BINARY, "IR_ts_edown")

SCORPION_END

SCORPION_INTERFACE(ir, range)

	/* Front/side IR (range) sensors: */
	SCORPION_(IR_BN_ENE,     IR_RANGE,  "IR_bn_ene")
	SCORPION_(IR_BN_WNW,     IR_RANGE,  "IR_bn_wnw")
	SCORPION_(IR_BN_N,       IR_RANGE,  "IR_bn_n")
	SCORPION_(IR_BN_NE,      IR_RANGE,  "IR_bn_ne")
	SCORPION_(IR_BN_NW,      IR_RANGE,  "IR_bn_nw")

	/* Front/east (range) sensors: */
	SCORPION_(IR_TE_NNW,     IR_RANGE,  "IR_te_nnw")
	SCORPION_(IR_TE_NNE,     IR_RANGE,  "IR_te_nne")

	/* Front/west (range) sensors: */
	SCORPION_(IR_TW_NNW,     IR_RANGE,  "IR_tw_nnw")
	SCORPION_(IR_TW_NNE,     IR_RANGE,  "IR_tw_nne")

	/* Back (range) sensors: */
	SCORPION_(IR_BS_W,       IR_RANGE,  "IR_bs_w")
	SCORPION_(IR_BS_E,       IR_RANGE,  "IR_bs_e")
	SCORPION_(IR_BW_S,       IR_RANGE,  "IR_bw_s")
	SCORPION_(IR_BE_S,       IR_RANGE,  "IR_be_s")

SCORPION_END
