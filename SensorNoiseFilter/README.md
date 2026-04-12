# Sensör Verisi Gürültü Temizleme (Sensor Noise Filter)

Titreşim sensöründen alınan verilerdeki gürültüyü temizlemek için geliştirilmiş Delphi uygulaması.

## Özellikler

- **CSV Dosyasından Veri Okuma:** `"serialNumber","x","y","z","time","deviceTime"` formatında veri yükleme
- **7 Farklı Filtre Algoritması:**
  - Hareketli Ortalama (Moving Average)
  - Medyan Filtre (Median Filter)
  - Düşük Geçişli Filtre (Low-Pass Filter)
  - Yüksek Geçişli Filtre (High-Pass Filter)
  - Bant Geçişli Filtre (Band-Pass Filter)
  - Kalman Filtresi (Kalman Filter)
  - Üstel Düzeltme (Exponential Smoothing)
- **Gürültü Tespiti:** Standart sapma tabanlı (sigma çarpanı ayarlanabilir)
- **Grafiksel Görselleştirme:** TPaintBox ile sinyal çizimi
  - Mavi: Ham veri noktaları
  - Yeşil: Filtrelenmiş veri
  - Kırmızı: Gürültü noktaları
- **İstatistiksel Analiz:** Ortalama, standart sapma, min/max değerleri
- **Filtrelenmiş Veri Dışa Aktarma:** CSV formatında kaydetme
- **MVC Mimarisi:** Temiz ve sürdürülebilir kod yapısı

## Proje Yapısı (MVC)

```
SensorNoiseFilter/
├── SystemAnalysis.md        # Sistem analizi dokümanı
├── SampleData.csv           # Örnek sensör verisi
├── README.md                # Bu dosya
└── src/
    ├── SensorNoiseFilter.dpr    # Ana proje dosyası
    ├── SensorDataModel.pas      # MODEL - Veri yapıları, CSV okuma, istatistik
    ├── SignalFilters.pas         # MODEL - Filtre algoritmaları
    ├── FilterController.pas     # CONTROLLER - İş mantığı koordinasyonu
    ├── MainForm.pas             # VIEW - Kullanıcı arayüzü
    └── MainForm.dfm             # VIEW - Form tasarımı
```

## Kullanım

1. Uygulamayı Delphi IDE'de açın (`src/SensorNoiseFilter.dpr`)
2. Projeyi derleyin ve çalıştırın
3. **CSV Yükle** butonu ile sensör verisi dosyasını seçin
4. Eksen seçin (X, Y veya Z)
5. Filtre türünü ve parametrelerini ayarlayın
6. **Filtre Uygula** butonuna tıklayın
7. Grafik üzerinde ham veri (mavi), filtrelenmiş veri (yeşil) ve gürültü noktalarını (kırmızı) görüntüleyin

## Filtre Parametreleri

| Filtre | Parametre 1 | Parametre 2 | Açıklama |
|--------|-------------|-------------|----------|
| Hareketli Ortalama | Pencere Boyutu (5) | - | 2-50 arası |
| Medyan Filtre | Pencere Boyutu (5) | - | 3-51 arası (tek sayı) |
| Düşük Geçişli | Alpha (0.1) | - | 0.01-1.0 arası |
| Yüksek Geçişli | Alpha (0.1) | - | 0.01-1.0 arası |
| Bant Geçişli | Alt Alpha (0.05) | Üst Alpha (0.3) | 0.01-1.0 arası |
| Kalman | Q-Süreç (0.01) | R-Ölçüm (0.1) | 0.001-10.0 arası |
| Üstel Düzeltme | Alpha (0.3) | - | 0.01-1.0 arası |

## Gereksinimler

- Embarcadero Delphi (RAD Studio) 10.x veya üzeri
- VCL Framework
