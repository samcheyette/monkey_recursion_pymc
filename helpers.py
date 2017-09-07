import random
import copy
import numpy as np

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
	lx = lambda x: -(x.count("(") + x.count("[") + 
					x.count("]") + x.count(")"))
	for h in sorted(out_hyps.keys(), key=lx):
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




def lst_format_data(paren_lst,  *args):

    all_resps = []
    assignments = []
    for a in xrange(len(args)):

        for part in args[a]:
            resps = []

            for i in xrange(len(paren_lst)):
                use_paren = tuple(paren_lst[i])

                if use_paren in part:
                    resps.append(part[use_paren])

                else:
                    resps.append(0)
            assignments.append(a)
            all_resps.append(copy.deepcopy(np.array(resps)))
    return np.array(all_resps), np.array(assignments)



def format_algs(paren_lst, algs, sm=1e-2):
    out = []
    for which in algs:
        alg =algs[which]
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



def group_vars(vars,  group_by):
    #returns a dictionary with
    #key=variable_type, value = tuple(names, values)


    names = list(vars.axes[0])
    values = list(vars)

    dct = {}

    for n in xrange(len(names)):
        name = names[n]
        for g in group_by:
            if g in name:
                if g not in dct:
                    dct[g] = []
                dct[g].append(n)

    ret = {}
    for d in dct.keys():
        min_i = min(dct[d])
        max_i = max(dct[d])
        ret[d] = (names[min_i:max_i+1], values[min_i:max_i+1])

    return ret

        
def get_algs_of_type(algorithms):
    #classifies algorithm type by
    #recursive (CE only), crossing (CE + CR)
    # or tail recursive (TR)
    CE = ["([])", "[()]"]
    CR = ["[(])", "([)]"]
    TR = ["[]()", "()[]"]
    types = []

    for a in algorithms:
        alg =algorithms[a]
        p_CE = 0.0
        p_CR = 0.0
        p_TR = 0.0
        p_OTHER = 0.0
        for paren_type in alg:
            if paren_type in CE:
                p_CE += alg[paren_type]
            elif paren_type in CR:
                p_CR += alg[paren_type]
            elif paren_type in TR:
                p_TR += alg[paren_type]
            else:
                p_OTHER += alg[paren_type]

        if p_CE == 1.0:
            types.append("Recursive")
        elif p_CE == 0.5 and p_CR == 0.5:
            types.append("Crossing")
        elif p_TR == 1.0:
            types.append("Tail")
        else:
            types.append("Other")

    return types

def amount_alg_type(alg_types, values_lst, which_type = "Recursive"): 
    #returns how much of X algorithm each participant or group is
    #(values is list of lists of floats)
    assert(len(values_lst) > 0)
    amounts = []
    for values in values_lst:
        assert(len(alg_types) == len(values))
        amount = 0.0
        for a in xrange(len(alg_types)):
            alg_type = alg_types[a]
            value = values[a]
            if alg_type == which_type:
                amount += value

        amounts.append(amount)

    return amounts

def output_alphas(names, means, sds, file):
    o = "who,mean,sd\n"
    for i in xrange(len(names)):
        name = names[i]
        mean = means[i]
        sd = sds[i]
        o += "%s,%f,%f\n" % (name, mean, sd)

    f = open(file, "w+")
    f.write(o)
    f.close()

def output_betas(names, group_alg, group_sds, order, alg_names, alg_types, file):
    o = "who, alg_name, alg_type, val,sd\n"
    for i in xrange(len(group_alg)):
        who = order[i]
        vals= group_alg[i]
        sds = group_sds[i]
        for j in xrange(len(vals)):
            alg_name = alg_names[j]
            alg_type =alg_types[j]
            val = vals[j]
            sd = sds[j]
            o += "%s,%s,%s,%f,%f\n" % (who,alg_name,alg_type,val,sd)

    f =open(file, "w+")
    f.write(o)
    f.close()


def output_thetas(names, part_alg, part_sds, order, 
        alg_names, alg_types, file):
    o = "who,id,alg_name,alg_type,val,sd\n"
    for i in xrange(len(part_alg)):
        who = order[i]
        vals= part_alg[i]
        sds = part_sds[i]

        for j in xrange(len(vals)):
            alg_name = alg_names[j]
            alg_type =alg_types[j]
            alg_sd = sds[j]
            val = vals[j]
            o += ("%s, %d, %s, %s, %f, %f\n" % 
                (who,i,alg_name,alg_type,val,alg_sd))

    f =open(file, "w+")
    f.write(o)
    f.close()


