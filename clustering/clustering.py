from sklearn.decomposition import NMF
from sklearn.feature_selection import VarianceThreshold
from sklearn.preprocessing import MinMaxScaler
import pandas as pd
import numpy as np
from collections import defaultdict
import seaborn as sns
from matplotlib import pyplot as plt


def get_nmf_w_h(feature_matrix, n_components=5, alpha=0.0, l1_ratio=0.0, max_iter=200):
    model = NMF(n_components=n_components, init='nndsvd', alpha=alpha, l1_ratio=l1_ratio, max_iter=max_iter)
    W = model.fit_transform(feature_matrix)
    H = model.components_
    reconstruction_err = model.reconstruction_err_
    return W, H, reconstruction_err


def feature_selection(X, p=0.):
    sel = VarianceThreshold(threshold=p)
    print('before feature selection: {} features'.format(X.shape[1]))
    X_after_feature_selection =  sel.fit_transform(X)
    print('after feature selection: {} features'.format(X_after_feature_selection.shape[1]))
    return X_after_feature_selection,sel.get_support(indices=True)

def get_nonzero_featurenames_in_clusters(H, threshold, feature_names):
    nonzero_features = defaultdict(list)
    for cluster_idx in range(H.shape[0]):
        nonzero_features[cluster_idx] = list(feature_names.reset_index(drop=True)[np.where(H[cluster_idx] > threshold)[0]])
    return nonzero_features

def get_nonzero_featurenames(cluster_nonzero_feature_map):
    res = set()
    for features in cluster_nonzero_feature_map.values():
        for f in features:
            res.add(f)
    return list(res)

def get_cluster_assignment(W):
    return np.argmax(W, axis=1)

def get_fraction_obs_with_non_zero_features(df, non_zero_features):
    relevant_df = df[non_zero_features]
    return relevant_df.sum(axis=0) / df.shape[0]

def plot_lift_matrix(df, non_zero_features):
    confidence = np.zeros((len(nonzero_features), len(nonzero_features)))

    for i, antecedent in enumerate(non_zero_features):
        for j, consequent in enumerate(non_zero_features):
            p_antecedent_consequent = df[(df[antecedent] == 1) & (df[consequent] == 1)].shape[0]
            p_antecedent = df[df[antecedent] == 1].shape[0]
            confidence[i, j] = p_antecedent_consequent / p_antecedent

    nonzero_features_short = [f.replace('drug_name_', '') for f in non_zero_features]
    confidence = pd.DataFrame(confidence, columns=nonzero_features_short, index=nonzero_features_short)

    p_consequent = get_fraction_obs_with_non_zero_features(df, nonzero_features)
    p_consequent.index = nonzero_features_short

    lift = confidence / p_consequent
    np.fill_diagonal(lift.values, 1)    # lift of x -> x is 1

    plt.clf()
    g = sns.heatmap(lift, square=True)

    fig = g.get_figure()
    fig.savefig('association_heatmap.png', bbox_inches='tight')


def plot_percent_feature_in_pop():
    # plots bar plot
    pass

def plot_ratio_feature_in_cluster_to_pop_matrix(df, non_zero_features):
    clusters = sorted(df.cluster.unique())
    ratios = np.zeros((len(nonzero_features), len(clusters)))

    def calc_ratio(cluster, cluster_idx):
        for i, feature in enumerate(non_zero_features):
            p_feature_cluster = cluster[cluster[feature] == 1].shape[0] / cluster.shape[0]
            p_feature = df[df[feature] == 1].shape[0] / df.shape[0]
            ratios[i, cluster_idx] = p_feature_cluster / p_feature

    for cluster_idx, cluster in df.groupby('cluster'):
        calc_ratio(cluster, cluster_idx)

    nonzero_features_short = [f.replace('drug_name_', '') for f in non_zero_features]
    ratios = pd.DataFrame(ratios, columns=clusters, index=nonzero_features_short)

    plt.clf()
    g = sns.heatmap(ratios)

    fig = g.get_figure()
    fig.savefig('feature_p_cluster_to_pop.png', bbox_inches='tight')

if __name__ == '__main__':
    #df = pd.read_csv('../data/data.csv')
    #df = pd.read_csv('../data/data_drug_names.csv')
    df = pd.read_csv('../data/data_questionnaire_for_clustering.csv')

    FEATURE_IMPORTANCE_THRESHOLD = 1

    # Only drug name cols
    del df['drug_name_55555']
    del df['drug_name_77777']
    del df['drug_name_99999']

    no_meds = df.drug_name_ == 1    # drug_name_ 1 means no medications
    df = df[~no_meds]
    del df['drug_name_']

    drug_name_cols = [c for c in df.columns if c.startswith('drug_name')]

    X, feature_indices_after_selection = feature_selection(df[drug_name_cols], p=.001)
    W, H, err = get_nmf_w_h(X, n_components=10)

    df['cluster'] = get_cluster_assignment(W)

    feature_names = pd.Series(drug_name_cols)[feature_indices_after_selection]

    nonzero_features_in_each_cluster = get_nonzero_featurenames_in_clusters(H, FEATURE_IMPORTANCE_THRESHOLD,
                                                                            feature_names)
    nonzero_features = get_nonzero_featurenames(nonzero_features_in_each_cluster)

    #plot_lift_matrix(df, nonzero_features)
    plot_ratio_feature_in_cluster_to_pop_matrix(df, nonzero_features)



