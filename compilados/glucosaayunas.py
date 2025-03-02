import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats

def analizar_estudios_imc(datos_estudios):
    """
    Analiza múltiples estudios de IMC con diferentes tamaños de muestra.
    
    Args:
        datos_estudios: Lista de diccionarios, donde cada diccionario contiene los datos de un estudio
                       con formato {'grupo': 'control'/'intervencion', 'n': tamaño_muestra, 
                                   'media': media_imc, 'desv_est': desviacion_estandar, 
                                   'min': valor_minimo, 'max': valor_maximo}
    
    Returns:
        Un diccionario con estadísticas combinadas para grupos control e intervención
    """
    # Separar datos por grupo
    control = [estudio for estudio in datos_estudios if estudio['grupo'] == 'control']
    intervencion = [estudio for estudio in datos_estudios if estudio['grupo'] == 'intervencion']
    
    # Calcular estadísticas combinadas para cada grupo
    resultados = {
        'control': calcular_estadisticas_combinadas(control),
        'intervencion': calcular_estadisticas_combinadas(intervencion),
        'diferencia': {}
    }
    
    # Calcular diferencia entre grupos
    resultados['diferencia']['media'] = resultados['intervencion']['media'] - resultados['control']['media']
    
    # Calcular intervalos de confianza para la diferencia
    n_control = sum(estudio['n'] for estudio in control)
    n_intervencion = sum(estudio['n'] for estudio in intervencion)
    
    # Error estándar de la diferencia
    se_diff = np.sqrt(
        (resultados['control']['desv_est']**2 / n_control) + 
        (resultados['intervencion']['desv_est']**2 / n_intervencion)
    )
    
    # Intervalo de confianza del 95%
    resultados['diferencia']['IC_95_inf'] = resultados['diferencia']['media'] - 1.96 * se_diff
    resultados['diferencia']['IC_95_sup'] = resultados['diferencia']['media'] + 1.96 * se_diff
    
    # Calcular valor p (aproximado con t-test)
    t_stat = resultados['diferencia']['media'] / se_diff
    df = n_control + n_intervencion - 2  # grados de libertad
    p_value = 2 * (1 - stats.t.cdf(abs(t_stat), df))
    resultados['diferencia']['p_valor'] = p_value
    
    return resultados

def calcular_estadisticas_combinadas(estudios):
    """
    Calcula estadísticas combinadas ponderadas por tamaño de muestra.
    """
    if not estudios:
        return None
    
    # Tamaño total de la muestra
    n_total = sum(estudio['n'] for estudio in estudios)
    
    # Media ponderada
    media_ponderada = sum(estudio['media'] * estudio['n'] for estudio in estudios) / n_total
    
    # Desviación estándar combinada (método de la varianza ponderada)
    # Primero calculamos la varianza combinada
    # Fórmula para combinar varianzas: Σ[(ni-1)*si² + ni*(mi-M)²] / (N-k)
    # donde ni = tamaño de la muestra i, si = desviación estándar de la muestra i
    # mi = media de la muestra i, M = media ponderada general, N = suma de todas las muestras, k = número de estudios
    
    sum_numerador = 0
    for estudio in estudios:
        # Contribución de la varianza interna de cada estudio
        sum_numerador += (estudio['n'] - 1) * (estudio['desv_est'] ** 2)
        # Contribución de la diferencia entre medias
        sum_numerador += estudio['n'] * ((estudio['media'] - media_ponderada) ** 2)
    
    # k es el número de estudios
    k = len(estudios)
    varianza_combinada = sum_numerador / (n_total - k)
    desv_est_combinada = np.sqrt(varianza_combinada)
    
    # Para mín y máx globales, tomamos los extremos
    min_global = min(estudio['min'] for estudio in estudios)
    max_global = max(estudio['max'] for estudio in estudios)
    
    return {
        'n': n_total,
        'media': media_ponderada,
        'desv_est': desv_est_combinada,
        'min': min_global,
        'max': max_global
    }

def visualizar_resultados(resultados):
    """
    Genera visualizaciones de los resultados del análisis.
    """
    # Crear DataFrame para facilitar la visualización
    df = pd.DataFrame({
        'Grupo': ['Control', 'Intervención'],
        'Media IMC': [resultados['control']['media'], resultados['intervencion']['media']],
        'Desv. Est.': [resultados['control']['desv_est'], resultados['intervencion']['desv_est']],
        'N': [resultados['control']['n'], resultados['intervencion']['n']]
    })
    
    # Configurar estilo de visualización
    sns.set_style("whitegrid")
    plt.figure(figsize=(12, 10))
    
    # Gráfico de barras para medias con barras de error
    plt.subplot(2, 2, 1)
    sns.barplot(x='Grupo', y='Media IMC', data=df, palette='Set2')
    plt.title('Medias de IMC por grupo')
    plt.errorbar(x=[0, 1], y=df['Media IMC'], yerr=df['Desv. Est.'], fmt='o', color='black')
    
    # Gráfico de distribución simulada
    plt.subplot(2, 2, 2)
    x = np.linspace(
        min(resultados['control']['min'], resultados['intervencion']['min']) - 1,
        max(resultados['control']['max'], resultados['intervencion']['max']) + 1, 
        1000
    )
    
    plt.plot(x, stats.norm.pdf(x, resultados['control']['media'], resultados['control']['desv_est']), 
             label=f"Control (n={resultados['control']['n']})")
    plt.plot(x, stats.norm.pdf(x, resultados['intervencion']['media'], resultados['intervencion']['desv_est']), 
             label=f"Intervención (n={resultados['intervencion']['n']})")
    plt.title('Distribución estimada de IMC')
    plt.xlabel('IMC')
    plt.ylabel('Densidad')
    plt.legend()
    
    # Gráfico de tamaño de efecto (forest plot simplificado)
    plt.subplot(2, 2, 3)
    plt.axvline(x=0, color='gray', linestyle='--')
    plt.errorbar(x=[resultados['diferencia']['media']], 
                 y=[0.5], 
                 xerr=[[resultados['diferencia']['media'] - resultados['diferencia']['IC_95_inf']], 
                        [resultados['diferencia']['IC_95_sup'] - resultados['diferencia']['media']]], 
                 fmt='o', color='black', capsize=5, markersize=10)
    plt.title('Diferencia entre grupos (IC 95%)')
    plt.xlabel('Diferencia en IMC (Intervención - Control)')
    plt.yticks([])
    
    # Tabla con resultados numéricos
    plt.subplot(2, 2, 4)
    plt.axis('off')
    tabla_texto = f"""
    RESULTADOS DEL ANÁLISIS:
    
    Grupo Control (n={resultados['control']['n']}):
      Media IMC: {resultados['control']['media']:.2f}
      Desv. Est.: {resultados['control']['desv_est']:.2f}
      Rango: {resultados['control']['min']:.2f} - {resultados['control']['max']:.2f}
    
    Grupo Intervención (n={resultados['intervencion']['n']}):
      Media IMC: {resultados['intervencion']['media']:.2f}
      Desv. Est.: {resultados['intervencion']['desv_est']:.2f}
      Rango: {resultados['intervencion']['min']:.2f} - {resultados['intervencion']['max']:.2f}
    
    Diferencia (Intervención - Control):
      {resultados['diferencia']['media']:.2f} (IC 95%: {resultados['diferencia']['IC_95_inf']:.2f} a {resultados['diferencia']['IC_95_sup']:.2f})
      Valor p: {resultados['diferencia']['p_valor']:.4f}
    """
    plt.text(0, 0.5, tabla_texto, fontsize=12)
    
    plt.tight_layout()
    plt.show()

# Ejemplo de uso con datos simulados
if __name__ == "__main__":
    # Datos de ejemplo (simulados)
    datos_ejemplo = [
        {'grupo': 'control', 'n': 50, 'media': 26.4, 'desv_est': 3.2, 'min': 20.1, 'max': 34.7},
        {'grupo': 'control', 'n': 75, 'media': 25.9, 'desv_est': 2.8, 'min': 19.8, 'max': 33.5},
        {'grupo': 'control', 'n': 40, 'media': 27.1, 'desv_est': 3.5, 'min': 21.3, 'max': 35.2},
        {'grupo': 'intervencion', 'n': 48, 'media': 24.7, 'desv_est': 3.0, 'min': 19.2, 'max': 32.8},
        {'grupo': 'intervencion', 'n': 80, 'media': 23.9, 'desv_est': 2.5, 'min': 18.5, 'max': 31.4},
        {'grupo': 'intervencion', 'n': 35, 'media': 24.3, 'desv_est': 2.7, 'min': 18.9, 'max': 30.8}
    ]
    
    # Realizar análisis
    resultados = analizar_estudios_imc(datos_ejemplo)
    
    # Imprimir resultados
    print("\nRESULTADOS DEL ANÁLISIS:")
    print("\nGrupo Control:")
    print(f"  N total: {resultados['control']['n']}")
    print(f"  Media ponderada: {resultados['control']['media']:.2f}")
    print(f"  Desviación estándar combinada: {resultados['control']['desv_est']:.2f}")
    print(f"  Mínimo global: {resultados['control']['min']:.2f}")
    print(f"  Máximo global: {resultados['control']['max']:.2f}")
    
    print("\nGrupo Intervención:")
    print(f"  N total: {resultados['intervencion']['n']}")
    print(f"  Media ponderada: {resultados['intervencion']['media']:.2f}")
    print(f"  Desviación estándar combinada: {resultados['intervencion']['desv_est']:.2f}")
    print(f"  Mínimo global: {resultados['intervencion']['min']:.2f}")
    print(f"  Máximo global: {resultados['intervencion']['max']:.2f}")
    
    print("\nDiferencia (Intervención - Control):")
    print(f"  Diferencia en medias: {resultados['diferencia']['media']:.2f}")
    print(f"  IC 95%: ({resultados['diferencia']['IC_95_inf']:.2f}, {resultados['diferencia']['IC_95_sup']:.2f})")
    print(f"  Valor p: {resultados['diferencia']['p_valor']:.4f}")
    
    # Visualizar resultados
    visualizar_resultados(resultados)
