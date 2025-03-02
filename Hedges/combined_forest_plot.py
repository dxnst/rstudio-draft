import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D

# Function to extract results from each dataset
def extract_results(estudios, categoria):
    """Calculate effect sizes and return results for a category"""
    resultados_estudios = []
    
    for estudio in estudios:
        n_control = estudio['n_control']
        n_intervencion = estudio['n_intervencion']
        media_control = estudio['media_control']
        media_intervencion = estudio['media_intervencion']
        de_control = estudio['de_control']
        de_intervencion = estudio['de_intervencion']
        nombre = estudio.get('nombre', f"Estudio")
        
        # Calculate mean difference
        diferencia_medias = media_intervencion - media_control
        
        # Calculate pooled standard deviation
        de_agrupada = np.sqrt(((n_control - 1) * de_control**2 + (n_intervencion - 1) * de_intervencion**2) / 
                             (n_control + n_intervencion - 2))
        
        # Calculate Cohen's d
        d_cohen = diferencia_medias / de_agrupada
        
        # Calculate Hedges' g (bias-corrected Cohen's d)
        factor_correccion = 1 - (3 / (4 * (n_control + n_intervencion - 2) - 1))
        g_hedges = d_cohen * factor_correccion
        
        # Calculate standard error for Hedges' g
        se_g_hedges = np.sqrt((n_control + n_intervencion) / (n_control * n_intervencion) + 
                             (g_hedges**2) / (2 * (n_control + n_intervencion - 2)))
        
        # Calculate 95% confidence interval
        ic_inferior = g_hedges - 1.96 * se_g_hedges
        ic_superior = g_hedges + 1.96 * se_g_hedges
        
        # Calculate study weight
        peso = 1 / (se_g_hedges**2)
        
        # Interpret effect size
        interpretacion = "Sin efecto"
        if abs(g_hedges) >= 0.2 and abs(g_hedges) < 0.5:
            interpretacion = "Pequeño"
        elif abs(g_hedges) >= 0.5 and abs(g_hedges) < 0.8:
            interpretacion = "Moderado"
        elif abs(g_hedges) >= 0.8:
            interpretacion = "Grande"
            
        resultados_estudios.append({
            'nombre': nombre,
            'categoria': categoria,
            'n_control': n_control,
            'n_intervencion': n_intervencion,
            'n_total': n_control + n_intervencion,
            'g_hedges': g_hedges,
            'se_g_hedges': se_g_hedges,
            'IC_95_inferior': ic_inferior,
            'IC_95_superior': ic_superior,
            'peso': peso,
            'interpretacion': interpretacion
        })
    
    # Create DataFrame
    df_estudios = pd.DataFrame(resultados_estudios)
    
    # Calculate combined effect (meta-analysis with fixed effects model)
    if len(df_estudios) > 0:
        suma_pesos = df_estudios['peso'].sum()
        efecto_combinado = sum(df_estudios['g_hedges'] * df_estudios['peso']) / suma_pesos
        se_combinado = np.sqrt(1 / suma_pesos)
        
        # Confidence interval for combined effect
        ic_combinado_inf = efecto_combinado - 1.96 * se_combinado
        ic_combinado_sup = efecto_combinado + 1.96 * se_combinado
        
        # Interpretation of combined effect
        interpretacion_combinada = "Sin efecto"
        if abs(efecto_combinado) >= 0.2 and abs(efecto_combinado) < 0.5:
            interpretacion_combinada = "Pequeño"
        elif abs(efecto_combinado) >= 0.5 and abs(efecto_combinado) < 0.8:
            interpretacion_combinada = "Moderado"
        elif abs(efecto_combinado) >= 0.8:
            interpretacion_combinada = "Grande"
        
        # Global statistics
        n_total_control = df_estudios['n_control'].sum()
        n_total_intervencion = df_estudios['n_intervencion'].sum()
        
        # Calculate heterogeneity (I² and Q)
        Q = sum(df_estudios['peso'] * (df_estudios['g_hedges'] - efecto_combinado)**2)
        df_stat = len(df_estudios) - 1  # degrees of freedom
        I_cuadrado = max(0, (Q - df_stat) / Q * 100) if Q > 0 else 0
        
        resultados = {
            'categoria': categoria,
            'efecto_combinado': efecto_combinado,
            'se_combinado': se_combinado,
            'IC_95_combinado_inf': ic_combinado_inf,
            'IC_95_combinado_sup': ic_combinado_sup,
            'interpretacion_combinada': interpretacion_combinada,
            'n_total_control': n_total_control,
            'n_total_intervencion': n_total_intervencion,
            'n_total': n_total_control + n_total_intervencion,
            'Q': Q,
            'df': df_stat,
            'I_cuadrado': I_cuadrado,
            'num_estudios': len(df_estudios)
        }
    else:
        resultados = {
            'categoria': categoria,
            'efecto_combinado': np.nan,
            'se_combinado': np.nan,
            'IC_95_combinado_inf': np.nan,
            'IC_95_combinado_sup': np.nan,
            'interpretacion_combinada': "N/A",
            'n_total_control': 0,
            'n_total_intervencion': 0,
            'n_total': 0,
            'Q': np.nan,
            'df': 0,
            'I_cuadrado': np.nan,
            'num_estudios': 0
        }
    
    return resultados, df_estudios

def visualizar_forest_plot_combinado(resultados_por_categoria, df_estudios_combinado):
    """Create a combined forest plot showing all categories"""
    # Get unique categories
    categorias = [resultado['categoria'] for resultado in resultados_por_categoria]
    
    # Count total number of rows needed (studies + category headers + combined effects)
    total_estudios = len(df_estudios_combinado)
    total_filas = total_estudios + len(categorias) * 2  # studies + header + combined effect for each category
    
    # Create figure with appropriate size
    fig = plt.figure(figsize=(15, max(10, total_filas * 0.35)))
    
    # Track current y-position
    y_pos_actual = total_filas
    
    # Dictionary for mapping categories to colors
    colores_categoria = {
        'Glucosa en ayunas': '#1f77b4',
        'Testosterona libre': '#ff7f0e',
        'HOMA-IR': '#2ca02c',
        'IMC': '#d62728',
        'Insulina en ayunas': '#9467bd',
        'Ciclos menstruales': '#8c564b',
        'Testosterona total': '#e377c2'
    }
    
    # Track positions for all studies
    y_positions = []
    effect_sizes = []
    lower_cis = []
    upper_cis = []
    labels = []
    colors = []
    es_combinado = []  # Flag to indicate if it's a combined effect
    sample_sizes = []
    
    # Process each category
    for categoria_idx, categoria in enumerate(categorias):
        # Get studies for this category
        estudios_categoria = df_estudios_combinado[df_estudios_combinado['categoria'] == categoria]
        resultado_categoria = next(r for r in resultados_por_categoria if r['categoria'] == categoria)
        
        # Add category header
        y_pos_actual -= 1
        y_positions.append(y_pos_actual)
        effect_sizes.append(0)  # Placeholder
        lower_cis.append(0)     # Placeholder
        upper_cis.append(0)     # Placeholder
        labels.append(f"**{categoria}**")  # Bold header
        colors.append('black')  # Header color
        es_combinado.append(-1)  # -1 indicates header
        sample_sizes.append(0)  # Placeholder
        
        # Add individual studies
        for _, estudio in estudios_categoria.iterrows():
            y_pos_actual -= 1
            y_positions.append(y_pos_actual)
            effect_sizes.append(estudio['g_hedges'])
            lower_cis.append(estudio['IC_95_inferior'])
            upper_cis.append(estudio['IC_95_superior'])
            labels.append(estudio['nombre'])
            colors.append(colores_categoria.get(categoria, 'blue'))
            es_combinado.append(0)  # 0 indicates individual study
            sample_sizes.append(estudio['n_total'])
        
        # Add combined effect for this category
        y_pos_actual -= 1
        y_positions.append(y_pos_actual)
        effect_sizes.append(resultado_categoria['efecto_combinado'])
        lower_cis.append(resultado_categoria['IC_95_combinado_inf'])
        upper_cis.append(resultado_categoria['IC_95_combinado_sup'])
        labels.append(f"Combinado ({resultado_categoria['num_estudios']} estudios, n={resultado_categoria['n_total']})")
        colors.append(colores_categoria.get(categoria, 'red'))
        es_combinado.append(1)  # 1 indicates combined effect
        sample_sizes.append(resultado_categoria['n_total'])
        
        # Add spacing between categories
        if categoria_idx < len(categorias) - 1:
            y_pos_actual -= 1
            y_positions.append(y_pos_actual)
            effect_sizes.append(0)  # Placeholder
            lower_cis.append(0)     # Placeholder
            upper_cis.append(0)     # Placeholder
            labels.append("")       # Empty label for spacing
            colors.append('none')   # No color
            es_combinado.append(-2)  # -2 indicates spacing
            sample_sizes.append(0)  # Placeholder
    
    # Convert to arrays
    y_positions = np.array(y_positions)
    effect_sizes = np.array(effect_sizes)
    lower_cis = np.array(lower_cis)
    upper_cis = np.array(upper_cis)
    es_combinado = np.array(es_combinado)
    
    # Calculate plot limits
    valid_indices = (es_combinado >= 0)
    if np.any(valid_indices):
        x_min = np.min(lower_cis[valid_indices]) * 1.2
        x_max = np.max(upper_cis[valid_indices]) * 1.2
        x_min = min(x_min, -0.5)
        x_max = max(x_max, 0.5)
    else:
        x_min, x_max = -2, 2
    
    # Plot forest plot
    ax = plt.gca()
    
    # Add zones for effect interpretation
    ax.axvspan(-0.2, 0.2, color='lightgray', alpha=0.3, zorder=1)
    ax.axvspan(0.2, 0.5, color='lightyellow', alpha=0.3, zorder=1)
    ax.axvspan(-0.5, -0.2, color='lightyellow', alpha=0.3, zorder=1)
    ax.axvspan(0.5, 0.8, color='navajowhite', alpha=0.3, zorder=1)
    ax.axvspan(-0.8, -0.5, color='navajowhite', alpha=0.3, zorder=1)
    ax.axvspan(0.8, x_max, color='lightgreen', alpha=0.3, zorder=1)
    ax.axvspan(x_min, -0.8, color='lightgreen', alpha=0.3, zorder=1)
    
    # Add vertical line at zero (no effect)
    ax.axvline(x=0, color='black', linestyle='-', linewidth=0.8, zorder=2)
    
    # Plot confidence intervals for studies and combined effects
    for i, (y, effect, lower, upper, es_type) in enumerate(zip(y_positions, effect_sizes, lower_cis, upper_cis, es_combinado)):
        if es_type >= 0:  # If it's a study or combined effect
            linewidth = 2 if es_type == 1 else 1.5
            linestyle = '--' if es_type == 1 else '-'
            color = 'red' if es_type == 1 else colors[i]
            
            # Plot confidence interval line
            ax.hlines(y=y, xmin=lower, xmax=upper, colors=color, linestyles=linestyle, linewidth=linewidth, zorder=3)
            
            # Plot effect size point
            if es_type == 1:
                # For combined effect, draw a red triangle
                ax.scatter(effect, y, marker='D', s=100, color='red', edgecolor='black', zorder=4)
            else:
                # Circle for individual study, size proportional to sample size
                size = min(max(sample_sizes[i]/5, 30), 150)  # Scale size between 30 and 150
                ax.scatter(effect, y, s=size, color=color, edgecolor='black', zorder=4)
    
    # Add text labels
    for i, (y, label, es_type) in enumerate(zip(y_positions, labels, es_combinado)):
        if es_type == -1:  # Category header
            ax.text(-x_max*1.1, y, label.replace("**", ""), ha='left', va='center', fontweight='bold', fontsize=12)
        elif es_type >= 0:  # Study or combined effect
            # Add effect size text
            if es_type == 1:  # Combined effect
                fontweight = 'bold'
                fontsize = 10
                
                # Find the corresponding category for this combined effect
                for resultado in resultados_por_categoria:
                    if resultado['num_estudios'] > 0 and f"Combinado ({resultado['num_estudios']} estudios" in label:
                        effect_text = f"{effect_sizes[i]:.2f} [{lower_cis[i]:.2f}, {upper_cis[i]:.2f}] - {resultado['interpretacion_combinada']}"
                        het_text = f"I²={resultado['I_cuadrado']:.1f}%, Q={resultado['Q']:.2f}"
                        break
                else:
                    effect_text = f"{effect_sizes[i]:.2f} [{lower_cis[i]:.2f}, {upper_cis[i]:.2f}]"
                    het_text = ""
                
                ax.text(-x_max*1.1, y, label, ha='left', va='center', fontweight=fontweight, fontsize=fontsize)
                ax.text(x_max*0.5, y, effect_text, ha='left', va='center', fontweight=fontweight, fontsize=fontsize)
                
                # Add heterogeneity info if available
                if het_text:
                    ax.text(x_max*0.5, y-0.3, het_text, ha='left', va='center', fontsize=8, fontstyle='italic')
            else:  # Individual study
                fontsize = 10
                effect_text = f"{effect_sizes[i]:.2f} [{lower_cis[i]:.2f}, {upper_cis[i]:.2f}]"
                ax.text(-x_max*1.1, y, label, ha='left', va='center')
                ax.text(x_max*0.5, y, effect_text, ha='left', va='center', fontsize=fontsize)
    
    # Add legend for interpretation
    legend_elements = [
        Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=10, label='Estudio individual'),
        Line2D([0], [0], marker='D', color='w', markerfacecolor='red', markersize=10, label='Efecto combinado'),
        mpatches.Patch(facecolor='lightgray', edgecolor='gray', alpha=0.3, label='Sin efecto (|g| < 0.2)'),
        mpatches.Patch(facecolor='lightyellow', edgecolor='gray', alpha=0.3, label='Efecto pequeño (0.2 ≤ |g| < 0.5)'),
        mpatches.Patch(facecolor='navajowhite', edgecolor='gray', alpha=0.3, label='Efecto moderado (0.5 ≤ |g| < 0.8)'),
        mpatches.Patch(facecolor='lightgreen', edgecolor='gray', alpha=0.3, label='Efecto grande (|g| ≥ 0.8)')
    ]
    ax.legend(handles=legend_elements, loc='upper right', bbox_to_anchor=(1, -0.02), ncol=3)
    
    # Set axis limits and labels
    ax.set_xlim(x_min, x_max)
    ax.set_ylim(min(y_positions)-1, max(y_positions)+1)
    ax.set_xlabel('Tamaño del efecto (g de Hedges)', fontsize=12)
    ax.set_title('Forest Plot: Eficacia del Inositol en Parámetros Clínicos', fontsize=14, fontweight='bold')
    
    # Remove y-axis ticks
    ax.set_yticks([])
    ax.set_yticklabels([])
    
    # Add grid for easier reading
    ax.grid(axis='x', linestyle='--', alpha=0.3)
    
    # Add favor labels
    plt.text(x_min*0.9, min(y_positions)-0.5, "Favorece control", ha='center', fontsize=10)
    plt.text(x_max*0.9, min(y_positions)-0.5, "Favorece inositol", ha='center', fontsize=10)
    
    plt.tight_layout()
    return fig

# Define datasets for each category
# 1. Glucosa en ayunas
glucosa_estudios = [
    {
        'nombre': 'Shokrpour, 2019',
        'n_control': 26,
        'n_intervencion': 27,
        'media_control': 94.8,
        'media_intervencion': 89.8,
        'de_control': 9.7,
        'de_intervencion': 8.5
    },
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 83.9,
        'media_intervencion': 88.0,
        'de_control': 7.5,
        'de_intervencion': 8.5
    },
    {
        'nombre': 'Nordio, 2021',
        'n_control': 20,
        'n_intervencion': 20,
        'media_control': 105,
        'media_intervencion': 99,
        'de_control': 0.93,
        'de_intervencion': 1.375
    }
]

# 2. Testosterona libre
free_test_estudios = [
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 2.5,
        'media_intervencion': 2.48,
        'de_control': 0.98,
        'de_intervencion': 1.06
    }
]

# 3. HOMA-IR
homa_estudios = [
    {
        'nombre': 'Shokrpour, 2019',
        'n_control': 26,
        'n_intervencion': 27,
        'media_control': 2.8,
        'media_intervencion': 2.6,
        'de_control': 0.7,
        'de_intervencion': 0.8
    },
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 1.68,
        'media_intervencion': 1.76,
        'de_control': 0.64,
        'de_intervencion': 0.99
    },
    {
        'nombre': 'Nordio, 2019',
        'n_control': 8,
        'n_intervencion': 8,
        'media_control': 5.05,
        'media_intervencion': 2.45,
        'de_control': 1.51,
        'de_intervencion': 0.68
    },
    {
        'nombre': 'Nordio, 2021',
        'n_control': 20,
        'n_intervencion': 20,
        'media_control': 4.4,
        'media_intervencion': 3,
        'de_control': 0.307,
        'de_intervencion': 0.263
    },
    {
        'nombre': 'Genazzani, 2019',
        'n_control': 24,
        'n_intervencion': 24,
        'media_control': 2.8,
        'media_intervencion': 1.9,
        'de_control': 0.6,
        'de_intervencion': 0.5
    }
]

# 4. IMC
imc_estudios = [
    {
        'nombre': 'Shokrpour, 2019',
        'n_control': 26,
        'n_intervencion': 27,
        'media_control': 27.1,
        'media_intervencion': 27.8,
        'de_control': 3.3,
        'de_intervencion': 3
    },
    {
        'nombre': 'Donne, 2019',
        'n_control': 21,
        'n_intervencion': 12,
        'media_control': 31.9,
        'media_intervencion': 31.8,
        'de_control': 5.2,
        'de_intervencion': 6 
    },
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 28.4,
        'media_intervencion': 26.4,
        'de_control': 1.7,
        'de_intervencion':1.7 
    },
    {
        'nombre': 'Nordio, 2019',
        'n_control': 8,
        'n_intervencion': 8,
        'media_control': 24.08,
        'media_intervencion': 23.91,
        'de_control': 3.0,
        'de_intervencion':2.9 
    },
    {
        'nombre': 'Nordio, 2021',
        'n_control': 20,
        'n_intervencion': 20,
        'media_control': 28,
        'media_intervencion': 26,
        'de_control': 0.625,
        'de_intervencion': 0.313 
    },      
    {
        'nombre': 'Genazzani, 2019',
        'n_control': 24,
        'n_intervencion': 24,
        'media_control': 28.4,
        'media_intervencion': 27.1,
        'de_control': 1.7,
        'de_intervencion': 1.3
    }
]

# 5. Insulina en ayunas
insulina_estudios = [
    {
        'nombre': 'Shokrpour, 2019',
        'n_control': 26,
        'n_intervencion': 27,
        'media_control': 11.9,
        'media_intervencion': 10.8,
        'de_control': 2.4,
        'de_intervencion': 3
    },
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 8,
        'media_intervencion': 7.9,
        'de_control': 3.1,
        'de_intervencion': 3.8
    },
    {
        'nombre': 'Nordio, 2021',
        'n_control': 20,
        'n_intervencion': 20,
        'media_control': 21,
        'media_intervencion': 15.5,
        'de_control': 1.56,
        'de_intervencion': 1.313
    },
    {
        'nombre': 'Genazzani, 2019',
        'n_control': 24,
        'n_intervencion': 24,
        'media_control': 12.6,
        'media_intervencion': 9.8,
        'de_control': 2.4,
        'de_intervencion': 1.8
    }
]

# 6. Ciclos menstruales
ciclos_estudios = [
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 1.6,
        'media_intervencion': 2.0,
        'de_control': 0.5,
        'de_intervencion': 0.7
    }
]

# 7. Testosterona total
test_total_estudios = [
    {
        'nombre': 'Troisi, 2019',
        'n_control': 15,
        'n_intervencion': 15,
        'media_control': 42.7,
        'media_intervencion': 34.98,
        'de_control': 9.91,
        'de_intervencion': 8.11
    },
    {
        'nombre': 'Nordio, 2021',
        'n_control': 20,
        'n_intervencion': 20,
        'media_control': 63,
        'media_intervencion': 37,
        'de_control': 11.8,
        'de_intervencion': 9.8
    },
    {
        'nombre': 'Genazzani, 2019',
        'n_control': 24,
        'n_intervencion': 24,
        'media_control': 54.3,
        'media_intervencion': 54.8,
        'de_control': 5.0,
        'de_intervencion': 7.0
    }
]

# Calculate effect sizes for each category
resultados_glucosa, df_glucosa = extract_results(glucosa_estudios, "Glucosa en ayunas")
resultados_free_test, df_free_test = extract_results(free_test_estudios, "Testosterona libre")
resultados_homa, df_homa = extract_results(homa_estudios, "HOMA-IR")
resultados_imc, df_imc = extract_results(imc_estudios, "IMC") 
resultados_insulina, df_insulina = extract_results(insulina_estudios, "Insulina en ayunas")
resultados_ciclos, df_ciclos = extract_results(ciclos_estudios, "Ciclos menstruales")
resultados_test_total, df_test_total = extract_results(test_total_estudios, "Testosterona total")

# Combine all results
todos_resultados = [
    resultados_glucosa,
    resultados_free_test,
    resultados_homa,
    resultados_imc,
    resultados_insulina,
    resultados_ciclos,
    resultados_test_total
]

# Combine all dataframes
df_combinado = pd.concat([
    df_glucosa, 
    df_free_test, 
    df_homa, 
    df_imc, 
    df_insulina, 
    df_ciclos, 
    df_test_total
])

# Sort categories by effect size magnitude (absolute value)
todos_resultados.sort(key=lambda x: abs(x['efecto_combinado']) if not np.isnan(x['efecto_combinado']) else 0, reverse=True)

# Create and save combined forest plot
fig = visualizar_forest_plot_combinado(todos_resultados, df_combinado)

# Print summary of results
print("\nRESULTADOS DEL META-ANÁLISIS POR CATEGORÍA:")
print("{:<25} {:<10} {:<25} {:<15} {:<10}".format("Categoría", "g de Hedges", "IC 95%", "Interpretación", "I²"))
print("-" * 85)

for resultado in todos_resultados:
    if not np.isnan(resultado['efecto_combinado']):
        print("{:<25} {:<10.2f} [{:<10.2f}, {:<10.2f}] {:<15} {:<10.1f}%".format(
            resultado['categoria'],
            resultado['efecto_combinado'],
            resultado['IC_95_combinado_inf'],
            resultado['IC_95_combinado_sup'],
            resultado['interpretacion_combinada'],
            resultado['I_cuadrado']
        ))

print("\nNota: Valores negativos indican reducción favorable en el grupo de inositol.")
print("      Valores positivos indican aumento favorable en el grupo de inositol.")

# Save the plot
plt.savefig('forest_plot_inositol_eficacia.png', dpi=300, bbox_inches='tight')
plt.show()
