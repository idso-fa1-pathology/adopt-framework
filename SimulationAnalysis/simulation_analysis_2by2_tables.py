import os
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from sklearn.metrics import cohen_kappa_score


def acceptance_status(row):
    r1 = row['R1-PathReview']
    r2 = row['R2-PathReview']

    # Check TissueUnsuitable first
    if r1 == 'TissueUnsuitable' or r2 == 'TissueUnsuitable':
        return 'Tissue Unsuitable'

    # Then check acceptance combinations
    if r1 == 'Accept' and r2 == 'Accept':
        return 'Both Accept'
    elif r1 == 'Accept' and r2 == 'Reject':
        return 'Only Pathologist1 Accepts'
    elif r2 == 'Accept' and r1 == 'Reject':
        return 'Only Pathologist2 Accepts'
    else:
        return 'Neither Accept'

 
def compute_agreement_kappa(series1, series2, label1, label2):
    mask = series1.notna() & series2.notna()
    agree = (series1[mask] == series2[mask]).mean()
    kappa = cohen_kappa_score(series1[mask], series2[mask])
    return {
        "Comparison": f"{label1} vs. {label2}",
        "N": mask.sum(),
        "Agreement %": round(agree * 100, 2),
        "Cohen's Kappa": round(kappa, 2)
    }


def plot_conf_matrices_with_stats(df, df_results, comparisons, display_labels):
    
    categories = ['Low', 'High']
    fig, axes = plt.subplots(1, 3, figsize=(10, 4))

    # Determine global max count for consistent scaling
    global_max = 0
    for row_col, col_col, _ in comparisons:
        cm = pd.crosstab(df[row_col], df[col_col])
        global_max = max(global_max, cm.values.max())

    green_cmap = plt.cm.Blues
    red_cmap = plt.cm.Reds
    norm = mcolors.Normalize(vmin=0, vmax=global_max)


    for ax, (row_col, col_col, idx) in zip(axes, comparisons):
        cm = pd.crosstab(df[row_col], df[col_col],
                         rownames=[display_labels.get(row_col, row_col)],
                         colnames=[display_labels.get(col_col, col_col)],
                         dropna=False).reindex(index=categories, columns=categories).fillna(0).astype(int)
        cm_total = cm.to_numpy().sum()
        cm_pct = cm / cm_total * 100

        ax.set_xlim(0, 2)
        ax.set_ylim(0, 2)
        ax.invert_yaxis()
        ax.set_xticks([0.5, 1.5])
        ax.set_yticks([0.5, 1.5])
        ax.set_xticklabels(categories, fontsize=11)
        ax.set_yticklabels(categories, fontsize=11, rotation=90)
        ax.set_xlabel(display_labels.get(col_col, col_col), fontsize=13)
        ax.set_ylabel(display_labels.get(row_col, row_col), fontsize=13)
        ax.set_title(df_results.loc[idx, 'Comparison'], fontsize=13, weight='bold', pad=12)
        ax.tick_params(length=0)

        for spine in ax.spines.values():
            spine.set_visible(False)

        # Draw confusion matrix cells
        for i, row_val in enumerate(categories):
            for j, col_val in enumerate(categories):
                count = cm.loc[row_val, col_val]
                pct = cm_pct.loc[row_val, col_val]
                text = f"{count}\n({pct:.1f}%)"
                base_cmap = green_cmap if i == j else red_cmap
                facecolor = base_cmap(norm(count))
                font_color = 'white' if norm(count) > 0.5 else 'black'
                rect = plt.Rectangle((j, i), 1, 1, facecolor=facecolor, edgecolor='white', linewidth=1.5)
                ax.add_patch(rect)
                ax.text(j + 0.5, i + 0.5, text, ha='center', va='center', fontsize=13, color=font_color)

        ax.text(0.5, -0.3, f"Agreement Rate: {df_results.loc[idx, 'Agreement %']}%", 
                ha='center', va='center', fontsize=13, transform=ax.transAxes)
        ax.text(0.5, -0.4, f"Cohen's Kappa: {df_results.loc[idx, 'Cohen\'s Kappa']}", 
                ha='center', va='center', fontsize=13, transform=ax.transAxes)
    
    plt.subplots_adjust(left=0.05, right=0.99, top=0.88, bottom=0.3, wspace=0.4)
    return fig, axes


if __name__ == "__main__":

    root_dir = "/Volumes/idso_fa1_pathology/codes/sranjbar/aistil-clia-module1/notebooks_for_ms_figures/simulation"
    external_file_path = os.path.join(root_dir, "spreadsheets/MDA-TNBC-External-86-AIsTIL-Review.xlsx")
    internal_file_path = os.path.join(root_dir, "spreadsheets/MDA-TNBC-Internal-80-AIsTIL-Review.xlsx")
    
    fig_dir = os.path.join(root_dir, "figures")
    os.makedirs(fig_dir, exist_ok=True)

    comparisons = [
        ('R2-sTIL', 'AI-sTIL', 0),  # Path #1 vs AI
        ('R1-sTIL', 'AI-sTIL', 1),  # Path #2 vs AI
        ('R2-sTIL', 'R1-sTIL', 2)   # Path #1 vs Path #2
    ]

    # -- external dataset
    external_display_labels = {
        'R1-sTIL': 'Pathologist #3',
        'R2-sTIL': 'Pathologist #1',
        'AI-sTIL': 'AI'
    }
    df = pd.read_excel(external_file_path, sheet_name='Sheet1')
    df['Acceptance_Status'] = df.apply(acceptance_status, axis=1)

    results_full = [
        compute_agreement_kappa(df['AI-sTIL'], df['R2-sTIL'], external_display_labels['AI-sTIL'], external_display_labels['R2-sTIL']),
        compute_agreement_kappa(df['AI-sTIL'], df['R1-sTIL'], external_display_labels['AI-sTIL'], external_display_labels['R1-sTIL']),
        compute_agreement_kappa(df['R2-sTIL'], df['R1-sTIL'], external_display_labels['R2-sTIL'], external_display_labels['R1-sTIL'])
    ]
    df_results_full = pd.DataFrame(results_full)

    fig, axes = plot_conf_matrices_with_stats(df, df_results_full, comparisons, external_display_labels)
    axes[2].set_title('Pathologist #1 vs. #3', fontsize=13, weight='bold', pad=12)
    plt.show()
    
    figpath = os.path.join(fig_dir, 'simulation_results_2x2tables_w_stats_validation.png')
    fig.savefig(figpath, dpi=300)


    # repeat on internal dataset
    internal_display_labels = {
        'R1-sTIL': 'Pathologist #2',
        'R2-sTIL': 'Pathologist #1',
        'AI-sTIL': 'AI'
    }
    df = pd.read_excel(internal_file_path, sheet_name='Sheet1')
    df['Acceptance_Status'] = df.apply(acceptance_status, axis=1)
    results_full = [
        compute_agreement_kappa(df['AI-sTIL'], df['R2-sTIL'], external_display_labels['AI-sTIL'], external_display_labels['R2-sTIL']),
        compute_agreement_kappa(df['AI-sTIL'], df['R1-sTIL'], external_display_labels['AI-sTIL'], external_display_labels['R1-sTIL']),
        compute_agreement_kappa(df['R2-sTIL'], df['R1-sTIL'], external_display_labels['R2-sTIL'], external_display_labels['R1-sTIL'])
    ]
    df_results_full = pd.DataFrame(results_full)
    fig, axes = plot_conf_matrices_with_stats(df, df_results_full, comparisons, internal_display_labels)
    axes[2].set_title('Pathologist #1 vs. #2', fontsize=13, weight='bold', pad=12)
    plt.show()
    
    figpath = os.path.join(fig_dir, 'simulation_results_2x2tables_w_stats_internal.png')
    fig.savefig(figpath, dpi=300)