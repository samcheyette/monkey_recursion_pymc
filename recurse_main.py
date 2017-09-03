
import numpy as np
import pymc3 as pm
import theano as T
from helpers import *
from clean_steves_data import *
import matplotlib.pyplot as plt




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



def format_algs(paren_lst, algs, sm=1e-3):
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




def model():
    global data
    data = data.flatten()

    with pm.Model() as m:
        alpha = pm.Exponential('alpha', 1.0, shape=(N_GROUPS,1))
        beta = pm.Dirichlet('beta', np.ones(N_ALGS), 
                            shape=(N_GROUPS,N_ALGS))

        theta = pm.Dirichlet('theta', alpha[assignments] * beta[assignments], 
                            shape=(TOTAL_PARTS,N_ALGS))


       # s = Categorical('s', algorithms, shape=(5,8))


        theta_resp = theta.dot(algorithms)

        #samp1 = random.randint(0, N_GROUPS*)

        theta_resp = (theta_resp.flatten() * 
                1/float(TOTAL_PARTS))

        algs = pm.Multinomial('algs', TOTAL_SAMPLES,
                            theta_resp,
                        shape=(TOTAL_SAMPLES),
                        observed=data)

        trace = pm.sample(200, burnin=10)

        map_ = pm.find_MAP()



    print_star("Model Finished!")
    #print_star("MAP", map_)
    summary = pm.summary(trace)
    print pm.summary(trace)

    fig, axs = plt.subplots(3, 2) # 3 RVs
    sv = pm.traceplot(trace, ax=axs)
   # plt.show(fig)
    fig.savefig("a2.png")
    #plt.savefig(sv,"a2.png")





    return map_








if __name__ == "__main__":

    #make parentheses lists, 
    #and hypotheses (e.g. OOMC)

    paren_lst = make_lists()
    hyps = make_lists(prims=['O','C','M', "(", "[", "]", ")"])
    gen = get_hyps_gen(hyps)

    filt = filter_hyps(copy.deepcopy(gen),
                         thresh=0.5, rem_dup=True)
    alg_names = [x for x in filt]

    ##############################################

    ##extract data from files
    careAbout = "Order pressed"
    monkey_data = getCountData("stevesdata/RecursionMonkey.csv", 

                                careAbout, "Monkeys",
                                subset={"Exposure": "2"})

    kids_data = getCountData("stevesdata/RecursionKids.csv", 
                                careAbout, "Kids")

    tsimane_data = getCountData("stevesdata/RecursionTsimane.csv", 
                                careAbout, "Tsimane")

    #################################################

    data_assignments = lst_format_data(paren_lst,
                 monkey_data,
                     kids_data, 
                     tsimane_data)

    data = data_assignments[0]
    assignments = data_assignments[1]
    algorithms = format_algs(paren_lst,filt)


    N_GROUPS = 3
    TOTAL_SAMPLES = np.sum(data)
    TOTAL_PARTS = len(data)
    N_ALGS = len(algorithms)
    #N_ALGS = len()


    print_star("TOTAL_SAMPLES", TOTAL_SAMPLES)

    print_star("N_ALGS",N_ALGS)
    print_star("TOTAL_PARTS", TOTAL_PARTS)

    model_out = model()

    order = ["monkey", "kid", "tsimane"]
    c = 0


    """
    mod_len = len(model_out["beta"][0])
    for a in xrange(mod_len):
        c = 0
        for m in model_out["beta"]:
            who = order[c]

            which_alg = alg_names[a]
            beta_val = m[a]
            print_star(who, which_alg, beta_val)

            c += 1
    """

