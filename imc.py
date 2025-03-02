import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def calcular_diferencia_grupos_estadisticas(estudios):
    """
    Calcula la diferencia media y su desviación estándar entre grupo control e intervención
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
        
        # Calcular desviación estándar de la diferencia (para grupos independientes)
        # Fórmula para la varianza de la diferencia entre medias de grupos independientes
        de_diferencia = np.sqrt((de_control**2 / n_control) + (de_intervencion**2 / n_intervencion))
        
        # Calcular el intervalo de confianza al 95% (aproximación normal)
        ic_inferior = diferencia_medias - 1.96 * de_diferencia
        ic_superior = diferencia_medias + 1.96 * de_diferencia
        
        # Calcular el peso del estudio (inversamente proporcional a la varianza)
        peso = 1 / (de_diferencia**2)
        
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
            'de_diferencia': de_diferencia,
            'IC_95_inferior': ic_inferior,
            'IC_95_superior': ic_superior,
            'peso': peso
        })
    
    # Crear DataFrame
    df_estudios = pd.DataFrame(resultados_estudios)
    
    # Calcular la estimación combinada (meta-análisis) con modelo de efectos fijos
    suma_pesos = df_estudios['peso'].sum()
    diferencia_combinada = sum(df_estudios['diferencia_medias'] * df_estudios['peso']) / suma_pesos
    de_combinada = np.sqrt(1 / suma_pesos)
    
    # Intervalo de confianza para la estimación combinada
    ic_combinado_inf = diferencia_combinada - 1.96 * de_combinada
    ic_combinado_sup = diferencia_combinada + 1.96 * de_combinada
    
    # Estadísticas globales
    n_total_control = df_estudios['n_control'].sum()
    n_total_intervencion = df_estudios['n_intervencion'].sum()
    
    resultados = {
        'diferencia_combinada': diferencia_combinada,
        'de_combinada': de_combinada,
        'IC_95_combinado': (ic_combinado_inf, ic_combinado_sup),
        'n_total_control': n_total_control,
        'n_total_intervencion': n_total_intervencion,
        'n_total': n_total_control + n_total_intervencion
    }
    
    return resultados, df_estudios

def visualizar_diferencia_grupos(resultados, df_estudios):
    """
    Crea un forest plot para visualizar las diferencias entre grupos
    """
    plt.figure(figsize=(12, 8))
    
    # Posiciones en el eje Y
    y_pos = list(range(len(df_estudios), 0, -1))
    
    # Graficar intervalos de confianza para cada estudio
    plt.hlines(y=y_pos, xmin=df_estudios['IC_95_inferior'], xmax=df_estudios['IC_95_superior'], colors='blue', lw=2)
    
    # Graficar estimación puntual para cada estudio
    tamaños = df_estudios['n_total'] / df_estudios['n_total'].max() * 100 + 20  # Escalar tamaños
    plt.scatter(df_estudios['diferencia_medias'], y_pos, s=tamaños, color='blue', edgecolor='black', zorder=3)
    
    # Graficar estimación combinada
    plt.axvline(x=resultados['diferencia_combinada'], color='red', linestyle='--', lw=1.5)
    
    # Graficar intervalo de confianza combinado
    ic_inf, ic_sup = resultados['IC_95_combinado']
    plt.axvspan(ic_inf, ic_sup, alpha=0.2, color='red')
    
    # Línea de no efecto
    plt.axvline(x=0, color='black', linestyle='-', lw=0.5)
    
    # Ejes y etiquetas
    plt.yticks(y_pos, df_estudios['nombre'])
    plt.xlabel('Diferencia de medias (Intervención - Control)')
    plt.title('Forest Plot: Diferencia entre Grupo Intervención y Grupo Control')
    
    # Añadir información de cada estudio
    for i, estudio in df_estudios.iterrows():
        # Añadir texto con el valor exacto de la diferencia y el intervalo de confianza
        texto = f"{estudio['diferencia_medias']:.2f} [{estudio['IC_95_inferior']:.2f}, {estudio['IC_95_superior']:.2f}]"
        plt.text(max(df_estudios['IC_95_superior']) * 1.1, y_pos[i], texto, va='center')
    
    # Añadir texto para la estimación combinada
    texto_combinado = f"Combinado: {resultados['diferencia_combinada']:.2f} [{ic_inf:.2f}, {ic_sup:.2f}]"
    plt.text(max(df_estudios['IC_95_superior']) * 1.1, 0, texto_combinado, va='center', fontweight='bold')
    
    # Ajustar límites del eje X
    plt.xlim(min(df_estudios['IC_95_inferior']) * 1.2, max(df_estudios['IC_95_superior']) * 1.5)
    plt.ylim(-1, max(y_pos) + 1)
    
    plt.grid(axis='x', linestyle='--', alpha=0.3)
    
    return plt.gcf()

# Ejemplo de uso
if __name__ == "__main__":
    # Datos ficticios de múltiples estudios (por ejemplo, reducción de IMC)
    estudios_ejemplo = [
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
            'de_intervencion'1.7: 
        },
        {
            'nombre': 'Singh, 2020',
            'n_control': 66,
            'n_intervencion': 66,
            'media_control': ,
            'media_intervencion': ,
            'de_control': ,
            'de_intervencion': 
        }
        {
            'nombre': 'Soldat-Stankovic, 2021',
            'n_control': 30,
            'n_intervencion': 30,
            'media_control': ,
            'media_intervencion': ,
            'de_control': ,
            'de_intervencion': 
        }

    ]
    
    # Calcular diferencias
    resultados, df_estudios = calcular_diferencia_grupos_estadisticas(estudios_ejemplo)
    
    # Mostrar resultados
    print("\nRESULTADOS DEL ANÁLISIS DE DIFERENCIA ENTRE GRUPOS:")
    print(f"Diferencia combinada: {resultados['diferencia_combinada']:.2f}")
    print(f"Desviación estándar de la diferencia combinada: {resultados['de_combinada']:.2f}")
    print(f"Intervalo de confianza 95%: ({resultados['IC_95_combinado'][0]:.2f}, {resultados['IC_95_combinado'][1]:.2f})")
    print(f"Muestra total: {resultados['n_total']} participantes ({resultados['n_total_control']} control, {resultados['n_total_intervencion']} intervención)")
    
    print("\nRESULTADOS POR ESTUDIO:")
    print(df_estudios[['nombre', 'n_total', 'diferencia_medias', 'de_diferencia', 'IC_95_inferior', 'IC_95_superior']])
    
    # Visualizar
    fig = visualizar_diferencia_grupos(resultados, df_estudios)
    plt.tight_layout()
    plt.show()
