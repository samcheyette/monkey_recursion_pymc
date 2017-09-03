import random
import copy

matching_set = [('(', ')'), ('[', ']')]
open_set = ["(", "["]
closed_set = [")", "]"]
all_p = ['(','[',']',')']


def print_star(*args):
    if len(args) > 0:
        for arg in args:
            print arg
        print "*" * 100
        print



def make_lists(prims=['(','[',']',')'], 
               resps=[""], length=4):

    if ((not len(resps) == 0) and 
               len(resps[0]) == length):
        return resps
    else:
        new_resps = []
        for r in resps:
            for p in prims:
                new_resp = r + p
                new_resps.append(new_resp)

        return make_lists(prims,new_resps,length)

def get_hyps_gen(hyps):
    r_dct = {}
    for hyp in hyps:
        hyp_gen = get_hyp_gen(hyp, 
            copy.copy(all_p),
            copy.copy(open_set), 
            copy.copy(closed_set))

        r_dct[hyp] = copy.deepcopy(hyp_gen)
    return r_dct



def get_hyp_gen(hyp, available,
				 open_available, 
				 closed_available, 
				 sofar=""):


	def find_match(m, available):
	    for r in available:
	        if (m, r) in matching_set or (r, m) in matching_set:
	            return r
	    return None
	
	if len(hyp) == 0:
		return {"":1.0}

	else:
		h = hyp[0]
		poss = {}
		if h in "([])" and h in available:
			poss[h] = 1.0

		elif h == "O" and len(open_available)>0:
			for o in open_available:
				poss[o] = 1/float(len(open_available))

		elif h == "C" and len(closed_available)>0:
			for o in closed_available:
				poss[o] = 1/float(len(closed_available))

		elif h == "M" and len(closed_available) > 0:
			for s in sofar[::-1]:
				match = find_match(s, available)
				if (match in closed_available):
					poss[match] = 1.0
					break

		if len(poss.keys()) == 0:
			poss["*"] = 1.0


		ret_dcts = {}
		for key in poss:
			new_av = copy.copy(available)
			new_opav = copy.copy(open_available)
			new_clav = copy.copy(closed_available)
			new_sofar = sofar + key
			if key in new_av:
				new_av.remove(key)
				if key in new_opav:
					new_opav.remove(key)
				else:
					new_clav.remove(key)

			from_here = get_hyp_gen(hyp[1:], 
									new_av,
									new_opav,
									new_clav,
									new_sofar)
			prob_key = poss[key]
			for k in from_here:
				prob_here =from_here[k]
				if key + k not in ret_dcts:
					ret_dcts[key + k] = 0.0
				ret_dcts[key+k] += prob_here * prob_key

		return ret_dcts



def filter_hyps(out_hyps, thresh=2.0, rem_dup=False):
	keep = {}
	kept = []
	for h in out_hyps:
		badness = 0.0
		ext = sorted([k for k in out_hyps[h]])
		for k in out_hyps[h]:
			badness += float(k.count("*")) * out_hyps[h][k]


		if ((badness < thresh) and
			((not rem_dup) or 
				(ext not in kept))):

			keep[h] = out_hyps[h]
			kept.append(copy.deepcopy(ext))


	return keep
