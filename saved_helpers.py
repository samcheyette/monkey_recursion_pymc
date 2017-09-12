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

def format_algs(paren_lst, algs, sm=1e-2):
    out = []
    for which in algs:
        alg =algs[which]
        print alg
        resp = []
        for i in xrange(len(paren_lst)):
            if paren_lst[i] in alg:
                resp.append(alg[paren_lst[i]] + sm)
            else:
                resp.append(sm)

        resp = np.array(resp)
        sum_r = np.sum(resp)
        normed = resp * (1.0/sum_r)
        out.append(normed)
    return np.array(out)


