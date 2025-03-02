import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def calcular_tamano_efecto(estudios):
    """
    Calcula el tamaño del efecto (d de Cohen, g de Hedges) entre grupo control e intervención
    usando estadísticas resumidas de múltiples estudios.
    
    Parámetros:
    estudios: Lista de diccionarios con las estadísticas resumidas de cada estudio
             Cada diccionario debe contener:
             - 'n_control': Tamaño de muestra del grupo control
             - 'n_intervencion': Tamaño de muestra del grupo intervención
             - 'media_control': Media del grupo control
             - 'media_intervencion': Media del grupo intervención
             - 'de_control': Desviación estándar del grupo control
             - 'de_intervencion': Desviación estándar del grupo intervención
             - 'nombre': Nombre o identificador del estudio (opcional)
    
    Retorna:
    tupla: (resultados, df_estudios)
    """
    resultados_estudios = []
    
    for estudio in estudios:
        n_control = estudio['n_control']
        n_intervencion = estudio['n_intervencion']
        media_control = estudio['media_control']
        media_intervencion = estudio['media_intervencion']
        de_control = estudio['de_control']
        de_intervencion = estudio['de_intervencion']
        nombre = estudio.get('nombre', f"Estudio {len(resultados_estudios)+1}")
        
        # Calcular diferencia de medias
        diferencia_medias = media_intervencion - media_control
        
        # Calcular desviación estándar agrupada (pooled)
        # Fórmula para SD agrupada: sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
        de_agrupada = np.sqrt(((n_control - 1) * de_control**2 + (n_intervencion - 1) * de_intervencion**2) / 
                             (n_control + n_intervencion - 2))
        
        # Calcular d de Cohen (usando SD agrupada)
        d_cohen = diferencia_medias / de_agrupada
        
        # Calcular g de Hedges (d de Cohen corregido para muestras pequeñas)
        # Factor de corrección: J = 1 - 3/(4(n1+n2-2)-1)
        factor_correccion = 1 - (3 / (4 * (n_control + n_intervencion - 2) - 1))
        g_hedges = d_cohen * factor_correccion
        
        # Calcular error estándar de g de Hedges
        # SE(g) = sqrt((n1+n2)/(n1*n2) + g^2/(2*(n1+n2-2)))
        se_g_hedges = np.sqrt((n_control + n_intervencion) / (n_control * n_intervencion) + 
                             (g_hedges**2) / (2 * (n_control + n_intervencion - 2)))
        
        # Calcular el intervalo de confianza al 95% para g de Hedges
        ic_inferior = g_hedges - 1.96 * se_g_hedges
        ic_superior = g_hedges + 1.96 * se_g_hedges
        
        # Calcular el peso del estudio (inversamente proporcional a la varianza)
        peso = 1 / (se_g_hedges**2)
        
        # Calcular interpretación del tamaño del efecto según Cohen
        interpretacion = "Sin efecto"
        if abs(g_hedges) >= 0.2 and abs(g_hedges) < 0.5:
            interpretacion = "Pequeño"
        elif abs(g_hedges) >= 0.5 and abs(g_hedges) < 0.8:
            interpretacion = "Moderado"
        elif abs(g_hedges) >= 0.8:
            interpretacion = "Grande"
            
        resultados_estudios.append({
            'nombre': nombre,
            'n_control': n_control,
            'n_intervencion': n_intervencion,
            'n_total': n_control + n_intervencion,
            'media_control': media_control,
            'media_intervencion': media_intervencion,
            'de_control': de_control,
            'de_intervencion': de_intervencion,
            'diferencia_medias': diferencia_medias,
            'de_agrupada': de_agrupada,
            'd_cohen': d_cohen,
            'g_hedges': g_hedges,
            'se_g_hedges': se_g_hedges,
            'IC_95_inferior': ic_inferior,
            'IC_95_superior': ic_superior,
            'peso': peso,
            'interpretacion': interpretacion
        })
    
    # Crear DataFrame
    df_estudios = pd.DataFrame(resultados_estudios)
    
    # Calcular el tamaño del efecto combinado (meta-análisis) con modelo de efectos fijos
    suma_pesos = df_estudios['peso'].sum()
    efecto_combinado = sum(df_estudios['g_hedges'] * df_estudios['peso']) / suma_pesos
    se_combinado = np.sqrt(1 / suma_pesos)
    
    # Intervalo de confianza para el tamaño del efecto combinado
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
    df = len(df_estudios) - 1  # grados de libertad
    I_cuadrado = max(0, (Q - df) / Q * 100) if Q > 0 else 0
    
    resultados = {
        'efecto_combinado': efecto_combinado,
        'se_combinado': se_combinado,
        'IC_95_combinado': (ic_combinado_inf, ic_combinado_sup),
        'interpretacion_combinada': interpretacion_combinada,
        'n_total_control': n_total_control,
        'n_total_intervencion': n_total_intervencion,
        'n_total': n_total_control + n_total_intervencion,
        'Q': Q,
        'df': df,
        'I_cuadrado': I_cuadrado
    }
    
    return resultados, df_estudios

def visualizar_tamano_efecto(resultados, df_estudios):
    """
    Crea un forest plot para visualizar el tamaño del efecto (g de Hedges)
    """
    plt.figure(figsize=(12, 8))
    
    # Posiciones en el eje Y
    y_pos = list(range(len(df_estudios), 0, -1))
    
    # Graficar intervalos de confianza para cada estudio
    plt.hlines(y=y_pos, xmin=df_estudios['IC_95_inferior'], xmax=df_estudios['IC_95_superior'], colors='blue', lw=2)
    
    # Graficar estimación puntual para cada estudio
    tamaños = df_estudios['n_total'] / df_estudios['n_total'].max() * 100 + 20  # Escalar tamaños
    plt.scatter(df_estudios['g_hedges'], y_pos, s=tamaños, color='blue', edgecolor='black', zorder=3)
    
    # Graficar estimación combinada
    plt.axvline(x=resultados['efecto_combinado'], color='red', linestyle='--', lw=1.5)
    
    # Graficar intervalo de confianza combinado
    ic_inf, ic_sup = resultados['IC_95_combinado']
    plt.axvspan(ic_inf, ic_sup, alpha=0.2, color='red')
    
    # Línea de no efecto
    plt.axvline(x=0, color='black', linestyle='-', lw=0.5)
    
    # Ejes y etiquetas
    plt.yticks(y_pos, df_estudios['nombre'])
    plt.xlabel('Tamaño del efecto (g de Hedges)')
    plt.title('Forest Plot: Tamaño del Efecto (g de Hedges)')
    
    # Añadir información de cada estudio
    for i, estudio in df_estudios.iterrows():
        # Añadir texto con el valor exacto del tamaño del efecto y el intervalo de confianza
        texto = f"{estudio['g_hedges']:.2f} [{estudio['IC_95_inferior']:.2f}, {estudio['IC_95_superior']:.2f}] - {estudio['interpretacion']}"
        plt.text(max(df_estudios['IC_95_superior']) * 1.1, y_pos[i], texto, va='center')
    
    # Añadir texto para la estimación combinada
    texto_combinado = f"Combinado: {resultados['efecto_combinado']:.2f} [{ic_inf:.2f}, {ic_sup:.2f}] - {resultados['interpretacion_combinada']}"
    plt.text(max(df_estudios['IC_95_superior']) * 1.1, 0, texto_combinado, va='center', fontweight='bold')
    
    # Añadir información sobre heterogeneidad
    texto_heterogeneidad = f"Heterogeneidad: I² = {resultados['I_cuadrado']:.1f}%, Q = {resultados['Q']:.2f}, df = {resultados['df']}"
    plt.text(max(df_estudios['IC_95_superior']) * 1.1, -1, texto_heterogeneidad, va='center', fontsize=10, fontstyle='italic')
    
    # Ajustar límites del eje X
    x_min = min(min(df_estudios['IC_95_inferior']) * 1.2, -0.5)
    x_max = max(max(df_estudios['IC_95_superior']) * 1.5, 0.5)
    plt.xlim(x_min, x_max)
    plt.ylim(-2, max(y_pos) + 1)
    
    # Añadir líneas de referencia para interpretación
    plt.axvspan(-0.2, 0.2, alpha=0.1, color='gray')
    plt.axvspan(0.2, 0.5, alpha=0.1, color='yellow')
    plt.axvspan(-0.5, -0.2, alpha=0.1, color='yellow')
    plt.axvspan(0.5, 0.8, alpha=0.1, color='orange')
    plt.axvspan(-0.8, -0.5, alpha=0.1, color='orange')
    plt.axvspan(0.8, x_max, alpha=0.1, color='green')
    plt.axvspan(x_min, -0.8, alpha=0.1, color='green')
    
    # Añadir leyenda de interpretación
    plt.text(x_min * 0.9, -1.5, "Sin efecto", ha='left', fontsize=8)
    plt.text(0.25, -1.5, "Pequeño", ha='center', fontsize=8)
    plt.text(0.65, -1.5, "Moderado", ha='center', fontsize=8)
    plt.text(x_max * 0.9, -1.5, "Grande", ha='right', fontsize=8)
    
    plt.grid(axis='x', linestyle='--', alpha=0.3)
    
    return plt.gcf()

# Ejemplo de uso
if __name__ == "__main__":
    # Datos de múltiples estudios (por ejemplo, reducción de IMC)
    estudios_ejemplo = [
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
    
    # Calcular tamaño del efecto
    resultados, df_estudios = calcular_tamano_efecto(estudios_ejemplo)
    
    # Mostrar resultados
    print("\nRESULTADOS DEL ANÁLISIS DE TAMAÑO DEL EFECTO:")
    print(f"Tamaño del efecto (g de Hedges) para efectividad de inositol: {resultados['efecto_combinado']:.2f}")
    print(f"Error estándar: {resultados['se_combinado']:.2f}")
    print(f"Intervalo de confianza 95%: ({resultados['IC_95_combinado'][0]:.2f}, {resultados['IC_95_combinado'][1]:.2f})")
    print(f"Interpretación: {resultados['interpretacion_combinada']}")
    print(f"Muestra total: {resultados['n_total']} participantes ({resultados['n_total_control']} control, {resultados['n_total_intervencion']} intervención)")
    print(f"Heterogeneidad: I² = {resultados['I_cuadrado']:.1f}%, Q = {resultados['Q']:.2f}, df = {resultados['df']}")
    
    print("\nRESULTADOS POR ESTUDIO:")
    print(df_estudios[['nombre', 'n_total', 'g_hedges', 'se_g_hedges', 'IC_95_inferior', 'IC_95_superior', 'interpretacion']])
    
    # Visualizar
    fig = visualizar_tamano_efecto(resultados, df_estudios)
    plt.tight_layout()
    plt.show()
