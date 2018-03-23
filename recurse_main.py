import pymc3 as pm
from helpers import *
import matplotlib.pyplot as plt
import seaborn as sns
#from pymc3.backends import Text




def model():
    global data
    alpha_prior = 10.
    beta_prior = 0.1
    alpha_init = np.ones((N_GROUPS,1))
    noise_init = np.ones((N_GROUPS,1))*1e-2

    parts_ones = np.ones((TOTAL_PARTS))
    data_ones = np.ones(len(data[0]))

    hds = store_hds_old(paren_lst,filt)
    ns = np.sum(data, axis=1)


    m_ass = np.where(assignments == 0)
    k_ass = np.where(assignments == 1)
    t_ass = np.where(assignments==2)
    a_ass = np.where(assignments==3)
    n_monk = len(m_ass[0])
    n_kid = len(k_ass[0])
    n_tsim = len(t_ass[0])
    n_adult = len(a_ass[0])

    smooth =  np.ones((TOTAL_PARTS,N_ALGS)) * beta_prior

    #bias in choice of starting parenthesis
    start_p = store_start_p(paren_lst, n=TOTAL_PARTS, lst = ["("])
    start_np = 1 - start_p

    with pm.Model() as m:
        alpha = pm.Exponential('alpha', alpha_prior,
                 shape=(N_GROUPS,1))
 
        alpha = np.ones((N_GROUPS, 1)) * 10.


        beta = pm.Dirichlet('beta', np.ones((N_GROUPS, N_ALGS))*beta_prior,
                        # testval=np.ones(N_ALGS),
                            shape=(N_GROUPS,N_ALGS)) 



        theta = pm.Dirichlet('theta',  alpha[assignments] * beta[assignments], 
                           shape=(TOTAL_PARTS,N_ALGS)) 


        #noise_pr_a = pm.Exponential('n_pr_a', 1.,shape=N_GROUPS)
        #noise_pr_b = pm.Exponential('n_pr_b', 1.,shape=N_GROUPS)

        #noise_pr_a = np.ones(N_GROUPS) * 10.
        #noise_pr_b = np.ones(N_GROUPS) * 10.


        noise = pm.Beta("noise", 1,2, shape=TOTAL_PARTS, testval=0.1)
        #noise = pm.Beta("noise", noise_pr_a[assignments],noise_pr_b[assignments], 
                    #    shape=TOTAL_PARTS)


       # noise = pm.Beta("noise", 1,1, shape=N_GROUPS, testval=0.1)




        new_algs = map(lambda x: theta[x].dot(format_algs_theano(hds, noise[x])), np.arange(TOTAL_PARTS))
        theta_resp = tt.concatenate([new_algs], axis=0)
        #theta_resp = theta.dot(algorithms)

        """
        noise = pm.Beta("noise", 1,9, shape=N_GROUPS, testval=0.1)
        noise_alg = algorithms + noise[assignments]
        new_algs = format_algs_theano_bypart(hds, noise, 
                                    total_parts=TOTAL_PARTS, 
                                   n_algs=N_ALGS,max_hd=max_hd)
        theta_resp = theta.dot(new_algs)

        #theta_resp = theta.dot(algorithms) 
        monkey_theta = theta[m_ass]
        kid_theta = theta[k_ass]
        tsim_theta = theta[t_ass]
        adult_theta = theta[a_ass]


        new_algs_monkey = format_algs_theano(hds, noise[0])
        new_algs_kid = format_algs_theano(hds, noise[1])
        new_algs_tsim = format_algs_theano(hds, noise[2])
        new_algs_adult = format_algs_theano(hds, noise[3])


        monkey_algs = monkey_theta.dot(new_algs_monkey)
        kid_algs = kid_theta.dot(new_algs_kid)
        tsim_algs = tsim_theta.dot(new_algs_tsim)
        adult_algs = adult_theta.dot(new_algs_adult)

        theta_resp = tt.concatenate([monkey_algs, kid_algs,
                 tsim_algs, adult_algs], axis=0)
        """
        bias = pm.Beta("bias", 1,1,shape=(TOTAL_PARTS,1))

        biased_theta_resps = start_p * bias * theta_resp + start_np * (1.-bias) * theta_resp
        sum_norm = biased_theta_resps.sum(axis=1).reshape((TOTAL_PARTS,1))
        biased_theta_resps = biased_theta_resps / sum_norm

        #biased_theta_resps = theta_resp

        pm.Multinomial('resp', n=ns, p = biased_theta_resps, 
               shape=(TOTAL_PARTS, N_RESPS), observed=data)

        #db = Text('trace')

        trace = pm.sample(MCMC_STEPS,
            tune=BURNIN,target_accept=0.9, thin=MCMC_THIN)
        print_star("Model Finished!")


    if MCMC_CHAINS > 1:
        print pm.gelman_rubin(trace)

    summary = pm.df_summary(trace)
    which = 45
    samp =100



    return trace, summary






if __name__ == "__main__":

    #make parentheses lists, 
    #and hypotheses (e.g. OOMC)

    MCMC_STEPS = 250
    MCMC_THIN = 10
    MCMC_CHAINS=1
    BURNIN = 25
    paren_lst = make_lists()
    hyps = make_lists(prims=["O","M", "C"])
    gen = get_hyps_gen(hyps)

    filt = filter_hyps(copy.deepcopy(gen),
                         thresh=0.5, rem_dup=True)

    alg_names = [x for x in filt]
    alg_types = get_algs_of_type(filt)


    ##############################################

    ##extract data from files
    careAbout = "Order pressed"
    

    monkey_data = getCountData("stevesdata/RecursionMonkey.csv", 

                                careAbout, "Monkeys",
                                subset={"Exposure": "2"})
    kids_data = getCountData("stevesdata/RecursionKids_MoreSubs.csv", 
                                careAbout, "Kids")


    kid_dig = getCountData("stevesdata/RecursionKids_MoreSubs.csv", 
                                "FORWARDS DIGITS", "Kids")
    adults_data = getCountData("stevesdata/RecursionAdults.csv", 
                                careAbout, "Adults")
    tsimane_data = getCountData("stevesdata/RecursionTsimane.csv", 
                                careAbout, "Tsimane")
        #################################################

    data_assignments = lst_format_data(paren_lst,
                 monkey_data,
                    kids_data, 
                     tsimane_data,adults_data)



    data = data_assignments[0]
    assignments = data_assignments[1]
    #groups = ["kids"]

    groups = ["monkeys", "kids","tsimane","adults"]
    alg_0 = get_0_columns(format_algs(paren_lst,filt, sm=0.0))

    dat_0 = get_0_columns(data)
    both_0 = list(alg_0.intersection(dat_0))

    paren_lst = np.delete(np.array(paren_lst), both_0)
    algorithms = format_algs(paren_lst,filt, sm=0.1)


    #data = np.delete(data, both_0, axis=1)


    data_assignments = lst_format_data(paren_lst,
                 monkey_data,
                     kids_data, 
                     tsimane_data,adults_data)

    data = data_assignments[0]
    assignments = data_assignments[1]
    ce_paren = [list(paren_lst).index("([])"), list(paren_lst).index("[()]")]
    ce_prob = []
    for d in data:
        ce_prob.append((d[ce_paren[0]] + d[ce_paren[1]])/float(sum(d)))

    #data_assignments = lst_format_data(paren_lst,
                   #  kids_data)


    print data
    #algorithms = np.delete(algorithms, both_0, axis=1)
    #algorithms =  algorithms/algorithms.sum(axis=1)[:,None]


    N_GROUPS = len(groups)
    TOTAL_SAMPLES = np.sum(data)
    TOTAL_PARTS = len(data)
    N_ALGS = len(algorithms)
    N_RESPS = len(algorithms[0])
    #N_ALGS = len()


    print_star("TOTAL_SAMPLES", TOTAL_SAMPLES)
    print_star("N_GROUPS", N_GROUPS)

    print_star("N_ALGS",N_ALGS)
    print_star("TOTAL_PARTS", TOTAL_PARTS)
    print_star("N_RESPS", N_RESPS)




    trace, model_out = model()


    means= model_out['mean']
    sds = model_out['sd']

    ###################################################




    grouped = group_vars(means, ["alpha", "beta", "theta", "noise"])
    alpha = grouped["alpha"]
    beta = grouped["beta"]
    theta = grouped["theta"]
    noise = grouped["noise"]
    print noise


    grouped_sds = group_vars(sds, ["alpha", "beta", "theta", "noise"])
    alpha_sd = grouped_sds["alpha"]
    beta_sd = grouped_sds["beta"]
    theta_sd = grouped_sds["theta"]
    noise_sd = grouped_sds["noise"]

    noise_names = noise[0]
    noise_vals = noise[1]
    noise_sds = noise_sd[1]
    alpha_names = alpha[0]
    alpha_vals = alpha[1]
    alpha_sds = alpha_sd[1]
    beta_names = np.array(beta[0]).reshape(N_GROUPS, N_ALGS)
    beta_vals = np.array(beta[1]).reshape(N_GROUPS, N_ALGS)
    beta_sds = np.array(beta_sd[1]).reshape(N_GROUPS, N_ALGS)
    theta_names = np.array(theta[0]).reshape(TOTAL_PARTS, N_ALGS)
    theta_vals = np.array(theta[1]).reshape(TOTAL_PARTS, N_ALGS)
    theta_sds = np.array(theta_sd[1]).reshape(TOTAL_PARTS, N_ALGS)


    noise = output_full_alpha_noise(trace, 'noise',  
        names=np.array(groups)[assignments],  thin=MCMC_THIN, 
        out="model_out/noise_full.csv")
    output_full_alpha_noise(trace, 'alpha',  
        names=groups, thin=MCMC_THIN, out="model_out/alpha_full.csv")
    output_full_theta_beta(trace, 'beta',  groups=groups,
        names=alg_names, thin=MCMC_THIN, out="model_out/beta_full.csv")
    output_full_theta_beta(trace, 'theta', groups=np.array(groups)[assignments],
        names=alg_names, thin=MCMC_THIN, out="model_out/theta_full.csv", added=noise) 

    output_alphas(groups, alpha_vals, alpha_sds, "model_out/alphas.csv")
    output_alphas(groups, noise_vals, noise_sds, "model_out/noise.csv")   
  
    output_betas(beta_names, beta_vals, beta_sds, groups, 
                    alg_names, alg_types,"model_out/betas.csv") 

    group_names = [groups[a] for a in assignments]
    output_thetas(beta_names, theta_vals, theta_sds, group_names, 
                    alg_names, alg_types,"model_out/thetas.csv")   

    recursive_betas = amount_alg_type(alg_types, beta_vals, 
                                    which_type="Recursive")
    crossing_betas = amount_alg_type(alg_types, beta_vals, 
                                    which_type="Crossing")
    tail_betas = amount_alg_type(alg_types, beta_vals, 
                                    which_type="Tail")

    print_star("Noise", groups, noise_vals)
    print_star("Recursive Betas", recursive_betas)
    print_star("Crossing Betas", crossing_betas)
    print_star("Tail Betas", tail_betas)



    #################################################
    print_star("Alpha", groups, alpha_vals)
    ind = np.arange(len(alpha_names))
    #width = 0.35

    fig, ax = plt.subplots()
    ax.bar(ind, alpha_vals)
    #ax.set_xticks(np.arange(len(alpha_names)))
    ax.set_xticks(ind)
    ax.set_xticklabels(groups)
    fig.savefig("alpha.png")

    ####################################################

    ind = np.arange(len(beta_names[0]))
    fig, ax = plt.subplots()
    ax.bar(ind, beta_vals[0])
    #ax.set_xticks(np.arange(len(alpha_names)))
    ax.set_xticks(ind)
    ax.set_xticklabels(alg_names)
    fig.savefig("beta.png")

    #####################################################


    #g = sns.factorplot(x=)