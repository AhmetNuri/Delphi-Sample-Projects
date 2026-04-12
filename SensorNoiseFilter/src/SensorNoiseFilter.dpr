{ ============================================================================
  SensorNoiseFilter.dpr - Ana Proje Dosyası

  Sensör Verisi Gürültü Temizleme Uygulaması
  
  Bu uygulama, titreşim sensöründen alınan verilerdeki gürültüyü temizlemek
  için çeşitli dijital filtreler uygular ve sonuçları görselleştirir.
  
  Desteklenen Filtreler:
    - Hareketli Ortalama (Moving Average)
    - Medyan Filtre (Median Filter)
    - Düşük Geçişli Filtre (Low-Pass Filter)
    - Yüksek Geçişli Filtre (High-Pass Filter)
    - Bant Geçişli Filtre (Band-Pass Filter)
    - Kalman Filtresi (Kalman Filter)
    - Üstel Düzeltme (Exponential Smoothing)
  
  Mimari: MVC (Model-View-Controller)
    Model:      SensorDataModel.pas, SignalFilters.pas
    View:       MainForm.pas
    Controller: FilterController.pas
  ============================================================================ }
program SensorNoiseFilter;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  SensorDataModel in 'SensorDataModel.pas',
  SignalFilters in 'SignalFilters.pas',
  FilterController in 'FilterController.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Sensör Verisi Gürültü Temizleme';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
