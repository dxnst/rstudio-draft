import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.lines import Line2D

# Función para extraer resultados de cada conjunto de datos
def extract_results(estudios, categoria):
    """Calcula tamaños del efecto y devuelve resultados para una categoría"""
    resultados_estudios = []
    
    for estudio in estudios:
        n_control = estudio['n_control']
        n_intervencion = estudio['n_intervencion']
        media_control = estudio['media_control']
        media_intervencion = estudio['media_intervencion']
        de_control = estudio['de_control']
        de_intervencion = estudio['de_intervencion']
        nombre = estudio.get('nombre', f"Estudio")
        
        # Calcular diferencia de medias
        diferencia_medias = media_intervencion - media_control
        
        # Calcular desviación estándar agrupada
        de_agrupada = np.sqrt(((n_control - 1) * de_control**2 + (n_intervencion - 1) * de_intervencion**2) / 
                             (n_control + n_intervencion - 2))
        
        # Calcular d de Cohen
        d_cohen = diferencia_medias / de_agrupada
        
        # Calcular g de Hedges (d de Cohen corregido por sesgo)
        factor_correccion = 1 - (3 / (4 * (n_control + n_intervencion - 2) - 1))
        g_hedges = d_cohen * factor_correccion
        
        # Calcular error estándar para g de Hedges
        se_g_hedges = np.sqrt((n_control + n_intervencion) / (n_control * n_intervencion) + 
                             (g_hedges**2) / (2 * (n_control + n_intervencion - 2)))
        
        # Calcular intervalo de confianza del 95%
        ic_inferior = g_hedges - 1.96 * se_g_hedges
        ic_superior = g_hedges + 1.96 * se_g_hedges
        
        # Calcular peso del estudio
        peso = 1 / (se_g_hedges**2)
        
        # Interpretar tamaño del efecto
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
    
    # Crear DataFrame
    df_estudios = pd.DataFrame(resultados_estudios)
    
    # Calcular efecto combinado (meta-análisis con modelo de efectos fijos)
    if len(df_estudios) > 0:
        suma_pesos = df_estudios['peso'].sum()
        efecto_combinado = sum(df_estudios['g_hedges'] * df_estudios['peso']) / suma_pesos
        se_combinado = np.sqrt(1 / suma_pesos)
        
        # Intervalo de confianza para el efecto combinado
        ic_combinado_inf = efecto_combinado - 1.96 * se_combinado
        ic_combinado_sup = efecto_combinado + 1.96 * se_combinado
        
        # Interpretación del efecto combinado
        interpretacion_combinada = "Sin efecto"
        if abs(efecto_combinado) >= 0.2 and abs(efecto_combinado) < 0.5:
            interpretacion_combinada = "Pequeño"
        elif abs(efecto_combinado) >= 0.5 and abs(efecto_combinado) < 0.8:
            interpretacion_combinada = "Moderado"
        elif abs(efecto_combinado) >= 0.8:
            interpretacion_combinada = "Grande"
        
        # Estadísticas globales
        n_total_control = df_estudios['n_control'].sum()
        n_total_intervencion = df_estudios['n_intervencion'].sum()
        
        # Calcular heterogeneidad (I² y Q)
        Q = sum(df_estudios['peso'] * (df_estudios['g_hedges'] - efecto_combinado)**2)
        df_stat = len(df_estudios) - 1  # grados de libertad
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

def visualizar_forest_plot_mejorado(resultados_por_categoria, df_estudios_combinado):
    """Crear un forest plot combinado que muestra todas las categorías 
    con nombres a la izquierda y valores a la derecha"""
    # Obtener categorías únicas
    categorias = [resultado['categoria'] for resultado in resultados_por_categoria]
    
    # Contar número total de filas necesarias (estudios + encabezados de categoría + efectos combinados)
    total_estudios = len(df_estudios_combinado)
    total_filas = total_estudios + len(categorias) * 2  # estudios + encabezado + efecto combinado para cada categoría
    
    # Crear figura con tamaño apropiado
    fig, ax = plt.subplots(figsize=(15, max(10, total_filas * 0.35)))
    
    # Controlar la posición y actual
    y_pos_actual = total_filas
    
    # Diccionario para mapear categorías a colores
    colores_categoria = {
        'Glucosa en ayunas': '#1f77b4',
        'Testosterona libre': '#ff7f0e',
        'HOMA-IR': '#2ca02c',
        'IMC': '#d62728',
        'Insulina en ayunas': '#9467bd',
        'Ciclos menstruales': '#8c564b',
        'Testosterona total': '#e377c2'
    }
    
    # Rastrear posiciones para todos los estudios
    y_positions = []
    effect_sizes = []
    lower_cis = []
    upper_cis = []
    study_names = []  # Nombres de estudios para el eje Y izquierdo
    effect_labels = []  # Etiquetas para los valores a la derecha
    colors = []
    es_combinado = []  # Bandera para indicar si es un efecto combinado
    sample_sizes = []
    
    # Procesar cada categoría
    for categoria_idx, categoria in enumerate(categorias):
        # Obtener estudios para esta categoría
        estudios_categoria = df_estudios_combinado[df_estudios_combinado['categoria'] == categoria]
        resultado_categoria = next(r for r in resultados_por_categoria if r['categoria'] == categoria)
        
        # Añadir encabezado de categoría
        y_pos_actual -= 1
        y_positions.append(y_pos_actual)
        effect_sizes.append(0)  # Marcador de posición
        lower_cis.append(0)     # Marcador de posición
        upper_cis.append(0)     # Marcador de posición
        study_names.append(f"{categoria}")  # Encabezado en negrita
        effect_labels.append("")  # Sin etiqueta en el lado derecho para encabezados
        colors.append('black')  # Color de encabezado
        es_combinado.append(-1)  # -1 indica encabezado
        sample_sizes.append(0)  # Marcador de posición
        
        # Añadir estudios individuales
        for _, estudio in estudios_categoria.iterrows():
            y_pos_actual -= 1
            y_positions.append(y_pos_actual)
            effect_sizes.append(estudio['g_hedges'])
            lower_cis.append(estudio['IC_95_inferior'])
            upper_cis.append(estudio['IC_95_superior'])
            study_names.append(estudio['nombre'])
            effect_text = f"{estudio['g_hedges']:.2f} [{estudio['IC_95_inferior']:.2f}, {estudio['IC_95_superior']:.2f}]"
            effect_labels.append(effect_text)
            colors.append(colores_categoria.get(categoria, 'blue'))
            es_combinado.append(0)  # 0 indica estudio individual
            sample_sizes.append(estudio['n_total'])
        
        # Añadir efecto combinado para esta categoría
        y_pos_actual -= 1
        y_positions.append(y_pos_actual)
        effect_sizes.append(resultado_categoria['efecto_combinado'])
        lower_cis.append(resultado_categoria['IC_95_combinado_inf'])
        upper_cis.append(resultado_categoria['IC_95_combinado_sup'])
        study_names.append(f"Combinado ({resultado_categoria['num_estudios']} estudios)")
        combined_effect_text = f"{resultado_categoria['efecto_combinado']:.2f} [{resultado_categoria['IC_95_combinado_inf']:.2f}, {resultado_categoria['IC_95_combinado_sup']:.2f}] - {resultado_categoria['interpretacion_combinada']}"
        effect_labels.append(combined_effect_text)
        colors.append(colores_categoria.get(categoria, 'red'))
        es_combinado.append(1)  # 1 indica efecto combinado
        sample_sizes.append(resultado_categoria['n_total'])
        
        # Añadir espacio entre categorías
        if categoria_idx < len(categorias) - 1:
            y_pos_actual -= 1
            y_positions.append(y_pos_actual)
            effect_sizes.append(0)  # Marcador de posición
            lower_cis.append(0)     # Marcador de posición
            upper_cis.append(0)     # Marcador de posición
            study_names.append("")  # Etiqueta vacía para espacio
            effect_labels.append("")  # Etiqueta vacía para espacio
            colors.append('none')   # Sin color
            es_combinado.append(-2)  # -2 indica espacio
            sample_sizes.append(0)  # Marcador de posición
    
    # Convertir a arrays
    y_positions = np.array(y_positions)
    effect_sizes = np.array(effect_sizes)
    lower_cis = np.array(lower_cis)
    upper_cis = np.array(upper_cis)
    es_combinado = np.array(es_combinado)
    
    # Calcular límites del gráfico
    valid_indices = (es_combinado >= 0)
    if np.any(valid_indices):
        x_min = np.min(lower_cis[valid_indices]) * 1.2
        x_max = np.max(upper_cis[valid_indices]) * 1.2
        x_min = min(x_min, -0.5)
        x_max = max(x_max, 0.5)
    else:
        x_min, x_max = -2, 2
    
    # Definir dimensiones para texto y gráfico
    fig_width = fig.get_figwidth()
    text_width_ratio = 0.25  # 25% del ancho para texto a la izquierda
    plot_width_ratio = 0.50  # 50% del ancho para el gráfico central
    values_width_ratio = 0.25  # 25% del ancho para valores a la derecha
    
    # Calcular límites para las tres secciones
    left_text_x = -0.22 * fig_width  # Límite izquierdo para texto
    right_text_x = 0.22 * fig_width  # Límite derecho para valores
    
    # Añadir zonas para interpretación del efecto
    ax.axvspan(-0.2, 0.2, color='lightgray', alpha=0.3, zorder=1)
    ax.axvspan(0.2, 0.5, color='lightyellow', alpha=0.3, zorder=1)
    ax.axvspan(-0.5, -0.2, color='lightyellow', alpha=0.3, zorder=1)
    ax.axvspan(0.5, 0.8, color='navajowhite', alpha=0.3, zorder=1)
    ax.axvspan(-0.8, -0.5, color='navajowhite', alpha=0.3, zorder=1)
    ax.axvspan(0.8, x_max, color='lightgreen', alpha=0.3, zorder=1)
    ax.axvspan(x_min, -0.8, color='lightgreen', alpha=0.3, zorder=1)
    
    # Añadir línea vertical en cero (sin efecto)
    ax.axvline(x=0, color='black', linestyle='-', linewidth=0.8, zorder=2)
    
    # Trazar intervalos de confianza para estudios y efectos combinados
    for i, (y, effect, lower, upper, es_type) in enumerate(zip(y_positions, effect_sizes, lower_cis, upper_cis, es_combinado)):
        if es_type >= 0:  # Si es un estudio o efecto combinado
            linewidth = 2 if es_type == 1 else 1.5
            linestyle = '--' if es_type == 1 else '-'
            color = 'red' if es_type == 1 else colors[i]
            
            # Trazar línea de intervalo de confianza
            ax.hlines(y=y, xmin=lower, xmax=upper, colors=color, linestyles=linestyle, linewidth=linewidth, zorder=3)
            
            # Trazar punto de tamaño del efecto
            if es_type == 1:
                # Para efecto combinado, dibujar un diamante rojo
                ax.scatter(effect, y, marker='D', s=100, color='red', edgecolor='black', zorder=4)
            else:
                # Círculo para estudio individual, tamaño proporcional al tamaño de la muestra
                size = min(max(sample_sizes[i]/5, 30), 150)  # Escalar tamaño entre 30 y 150
                ax.scatter(effect, y, s=size, color=color, edgecolor='black', zorder=4)
    
    # Añadir etiquetas de texto a la izquierda y valores a la derecha
    for i, (y, name, effect_label, es_type) in enumerate(zip(y_positions, study_names, effect_labels, es_combinado)):
        if es_type == -1:  # Encabezado de categoría
            ax.text(-0.25, y, name, ha='left', va='center', fontweight='bold', fontsize=12, transform=ax.transAxes)
        elif es_type == 0:  # Estudio individual
            ax.text(-0.25, y, name, ha='left', va='center', fontsize=10, transform=ax.transAxes)
            ax.text(1.02, y, effect_label, ha='left', va='center', fontsize=10, transform=ax.transAxes)
        elif es_type == 1:  # Efecto combinado
            ax.text(-0.25, y, name, ha='left', va='center', fontweight='bold', fontsize=10, transform=ax.transAxes)
            ax.text(1.02, y, effect_label, ha='left', va='center', fontweight='bold', fontsize=10, transform=ax.transAxes)
            
            # Añadir info de heterogeneidad si está disponible
            for resultado in resultados_por_categoria:
                if resultado['num_estudios'] > 0 and resultado['categoria'] in name:
                    het_text = f"I²={resultado['I_cuadrado']:.1f}%, Q={resultado['Q']:.2f}"
                    ax.text(1.02, y-0.3, het_text, ha='left', va='center', fontsize=8, fontstyle='italic', transform=ax.transAxes)
                    break
    
    # Añadir leyenda para interpretación
    legend_elements = [
        Line2D([0], [0], marker='o', color='w', markerfacecolor='gray', markersize=10, label='Estudio individual'),
        Line2D([0], [0], marker='D', color='w', markerfacecolor='red', markersize=10, label='Efecto combinado'),
        mpatches.Patch(facecolor='lightgray', edgecolor='gray', alpha=0.3, label='Sin efecto (|g| < 0.2)'),
        mpatches.Patch(facecolor='lightyellow', edgecolor='gray', alpha=0.3, label='Efecto pequeño (0.2 ≤ |g| < 0.5)'),
        mpatches.Patch(facecolor='navajowhite', edgecolor='gray', alpha=0.3, label='Efecto moderado (0.5 ≤ |g| < 0.8)'),
        mpatches.Patch(facecolor='lightgreen', edgecolor='gray', alpha=0.3, label='Efecto grande (|g| ≥ 0.8)')
    ]
    ax.legend(handles=legend_elements, loc='upper center', bbox_to_anchor=(0.5, -0.05), ncol=3)
    
    # Establecer límites y etiquetas de ejes
    ax.set_xlim(x_min, x_max)
    ax.set_ylim(min(y_positions)-1, max(y_positions)+1)
    ax.set_xlabel('Tamaño del efecto (g de Hedges)', fontsize=12)
    ax.set_title('Forest Plot: Eficacia del Inositol en Parámetros Clínicos', fontsize=14, fontweight='bold')
    
    # Quitar marcas del eje Y
    ax.set_yticks([])
    ax.set_yticklabels([])
    
    # Añadir cuadrícula para facilitar la lectura
    ax.grid(axis='x', linestyle='--', alpha=0.3)
    
    # Añadir etiquetas de favorecimiento
    plt.text(x_min*0.9, min(y_positions)-0.5, "Favorece control", ha='center', fontsize=10)
    plt.text(x_max*0.9, min(y_positions)-0.5, "Favorece inositol", ha='center', fontsize=10)
    
    # Ajustar layout
    plt.tight_layout()
    plt.subplots_adjust(left=0.25, right=0.75)  # Ajustar para dar espacio al texto
    
    return fig

# Definir conjuntos de datos para cada categoría
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

# Calcular tamaños del efecto para cada categoría
resultados_glucosa, df_glucosa = extract_results(glucosa_estudios, "Glucosa en ayunas")
resultados_free_test, df_free_test = extract_results(free_test_estudios, "Testosterona libre")
resultados_homa, df_homa = extract_results(homa_estudios, "HOMA-IR")
resultados_imc, df_imc = extract_results(imc_estudios, "IMC") 
resultados_insulina, df_insulina = extract_results(insulina_estudios, "Insulina en ayunas")
resultados_ciclos, df_ciclos = extract_results(ciclos_estudios, "Ciclos menstruales")
resultados_test_total, df_test_total = extract_results(test_total_estudios, "Testosterona total")

# Combinar todos los resultados en listas
resultados_por_categoria = [
    resultados_glucosa,
    resultados_free_test,
    resultados_homa,
    resultados_imc,
    resultados_insulina,
    resultados_ciclos,
    resultados_test_total
]

# Combinar todos los dataframes de estudios
df_estudios_combinado = pd.concat([
    df_glucosa,
    df_free_test,
    df_homa,
    df_imc,
    df_insulina,
    df_ciclos,
    df_test_total
])

# Crear el forest plot combinado
fig = visualizar_forest_plot_mejorado(resultados_por_categoria, df_estudios_combinado)

# Guardar la figura
plt.savefig('forest_plot_inositol.png', dpi=300, bbox_inches='tight')
plt.close()

# Mostrar un resumen de los resultados en formato tabla
resultados_resumen = []
for resultado in resultados_por_categoria:
    resultados_resumen.append({
        'Categoría': resultado['categoria'],
        'Estudios': resultado['num_estudios'],
        'Participantes': resultado['n_total'],
        'Efecto (g)': f"{resultado['efecto_combinado']:.2f} [{resultado['IC_95_combinado_inf']:.2f}, {resultado['IC_95_combinado_sup']:.2f}]",
        'Interpretación': resultado['interpretacion_combinada'],
        'I²': f"{resultado['I_cuadrado']:.1f}%"
    })

df_resumen = pd.DataFrame(resultados_resumen)
print("\nRESUMEN DE RESULTADOS DEL META-ANÁLISIS:")
print(df_resumen.to_string(index=False))

# Ejecutar el análisis completo
if __name__ == "__main__":
    print("Análisis meta-analítico de la eficacia del inositol en parámetros clínicos")
    print(f"Total de estudios analizados: {len(df_estudios_combinado)}")
    print(f"Total de categorías de resultados: {len(resultados_por_categoria)}")
    
    # Reportar hallazgos principales
    print("\nHallazgos principales:")
    for resultado in resultados_por_categoria:
        if resultado['num_estudios'] > 0:
            signo = "+" if resultado['efecto_combinado'] > 0 else "-"
            print(f"- {resultado['categoria']} ({resultado['num_estudios']} estudios, n={resultado['n_total']}): " 
                  f"g={resultado['efecto_combinado']:.2f} [{resultado['IC_95_combinado_inf']:.2f}, {resultado['IC_95_combinado_sup']:.2f}], "
                  f"efecto {signo}{resultado['interpretacion_combinada'].lower()}, I²={resultado['I_cuadrado']:.1f}%")
