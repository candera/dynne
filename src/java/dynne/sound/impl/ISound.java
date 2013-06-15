package dynne.sound.impl;

public interface ISound 
{
    public long channels();
    public double duration();
    public double amplitude(double t, long c);
}
